/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMorphologicalContourInterpolator_hxx
#define itkMorphologicalContourInterpolator_hxx

#include "itkMorphologicalContourInterpolator.h"
#include "itkObjectFactory.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageAlgorithm.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include <climits>
#include <utility>
#include <algorithm>
#include <queue>
#include <thread>
#include <future>
#include <chrono>

// DEBUG
#include <iostream>
#include "itkImageFileWriter.h"


namespace itk
{
template <typename TImage>
void
WriteDebug(itk::SmartPointer<TImage> out, const char * filename)
{
  return; // tests run much faster
  typedef ImageFileWriter<TImage> WriterType;
  typename WriterType::Pointer    w = WriterType::New();
  w->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  w->SetInput(out);
  w->SetFileName(filename);
  try
  {
    w->Update();
  }
  catch (ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
  }
}

template <unsigned int dim>
void
WriteDebug(itk::SmartPointer<Image<bool, dim>> out, const char * filename)
{
  typedef Image<bool, dim>                               BoolImageType;
  typedef Image<unsigned char, dim>                      ucharImageType;
  typedef CastImageFilter<BoolImageType, ucharImageType> CastType;
  typename CastType::Pointer                             caster = CastType::New();
  caster->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  caster->SetInput(out);
  WriteDebug<ucharImageType>(caster->GetOutput(), filename);
}

template <typename TImage>
bool
ImagesEqual(typename TImage::Pointer a, typename TImage::Pointer b)
{
  ImageRegionConstIterator<TImage> ita(a, a->GetLargestPossibleRegion());
  ImageRegionConstIterator<TImage> itb(b, b->GetLargestPossibleRegion());

  while (!ita.IsAtEnd())
  {
    if (ita.Get() != itb.Get())
    {
      break;
    }
    ++ita;
    ++itb;
  }

  if (ita.IsAtEnd())
  {
    return true;
  }
  else
  {
    return false;
  }
}


template <typename TImage>
MorphologicalContourInterpolator<TImage>::MorphologicalContourInterpolator()
  : m_Label(0)
  , m_Axis(-1)
  , m_HeuristicAlignment(true)
  , m_UseDistanceTransform(true)
  , m_ThreadPool(nullptr)
  , m_StopSpawning(false)
  , m_MinAlignIters(pow(2, TImage::ImageDimension))
  , // smaller of this and pixel count of the search image
  m_MaxAlignIters(pow(6, TImage::ImageDimension))
  ,                                       // bigger of this and root of pixel count of the search image
  m_LabeledSlices(TImage::ImageDimension) // initialize with empty sets
{
  // set up pipeline for regioned connected components
  m_RoI = RoiType::New();
  m_Binarizer = BinarizerType::New();
  m_Binarizer->SetInput(m_RoI->GetOutput());
  m_ConnectedComponents = ConnectedComponentsType::New();
  m_ConnectedComponents->SetInput(m_Binarizer->GetOutput());
  // FullyConnected is related to structuring element used
  // true for ball, false for cross
  m_ConnectedComponents->SetFullyConnected(true);
}


template <typename TImage>
typename MorphologicalContourInterpolator<TImage>::SliceSetType
MorphologicalContourInterpolator<TImage>::GetLabeledSliceIndices(unsigned int axis)
{
  return m_LabeledSlices[axis];
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::SetLabeledSliceIndices(unsigned int axis, SliceSetType indices)
{
  m_LabeledSlices[axis] = indices;
  this->Modified();
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::SetLabeledSliceIndices(unsigned int                                 axis,
                                                                 std::vector<typename TImage::IndexValueType> indices)
{
  m_LabeledSlices[axis] = SliceSetType().insert(indices.begin(), indices.end());
  this->Modified();
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::ExpandRegion(typename TImage::RegionType & region,
                                                       typename TImage::IndexType    index)
{
  for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
  {
    if (region.GetIndex(a) > index[a])
    {
      region.SetSize(a, region.GetSize(a) + region.GetIndex(a) - index[a]);
      region.SetIndex(a, index[a]);
    }
    else if (region.GetIndex(a) + (typename TImage::IndexValueType)region.GetSize(a) <= index[a])
    {
      region.SetSize(a, index[a] - region.GetIndex(a) + 1);
    }
    // else it is already within
  }
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::DetermineSliceOrientations()
{
  m_LabeledSlices.clear();
  m_LabeledSlices.resize(TImage::ImageDimension); // initialize with empty sets
  m_BoundingBoxes.clear();
  m_Orientations.clear();

  typename TImage::RegionType               region = m_Output->GetRequestedRegion();
  ImageRegionConstIteratorWithIndex<TImage> it(m_Input, region);

  OrientationType orientations = OrientationType();
  orientations.Fill(false);

  for (; !it.IsAtEnd(); ++it)
  {
    typename TImage::IndexType       indPrev, indNext;
    const typename TImage::IndexType ind = it.GetIndex();
    const typename TImage::PixelType val = m_Input->GetPixel(ind);
    if (val != 0 || (m_Label != 0 && val == m_Label))
    {
      typename TImage::RegionType boundingBox1;
      boundingBox1.SetIndex(ind);
      for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
      {
        boundingBox1.SetSize(a, 1);
      }
      std::pair<typename BoundingBoxesType::iterator, bool> resBB =
        m_BoundingBoxes.insert(std::make_pair(val, boundingBox1));
      if (!resBB.second) // include this index in existing BB
      {
        ExpandRegion(resBB.first->second, ind);
      }

      std::pair<typename OrientationsType::iterator, bool> res =
        m_Orientations.insert(std::make_pair(val, orientations));
      typename OrientationsType::iterator oRef = res.first;
      unsigned int                        cTrue = 0;
      unsigned int                        cAdjacent = 0;
      unsigned int                        axis = 0;
      for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
      {
        indPrev = ind;
        indPrev[a]--;
        indNext = ind;
        indNext[a]++;
        typename TImage::PixelType prev = 0;
        if (region.IsInside(indPrev))
        {
          prev = m_Input->GetPixel(indPrev);
        }
        typename TImage::PixelType next = 0;
        if (region.IsInside(indNext))
        {
          next = m_Input->GetPixel(indNext);
        }
        if (prev == 0 && next == 0) //&& - isolated slices only, || - flat edges too
        {
          axis = a;
          ++cTrue;
        }
        else if (prev == val && next == val)
        {
          ++cAdjacent;
        }
      }
      if (cTrue == 1 && cAdjacent == TImage::ImageDimension - 1) // slice has empty adjacent space only along one axis
      {
        oRef->second[axis] = true; // add this dimension for this label
        if (m_Axis == -1 || m_Axis == axis)
        {
          m_LabeledSlices[axis][val].insert(ind[axis]);
        }
      }
    }
  }
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::Extrapolate(int                             axis,
                                                      TImage *                        out,
                                                      typename TImage::PixelType      label,
                                                      typename TImage::IndexValueType i,
                                                      typename TImage::IndexValueType j,
                                                      typename TImage::Pointer        iConn,
                                                      typename TImage::PixelType      iRegionId)
{
  PixelList jRegionIds;
  jRegionIds.push_back(iRegionId);
  typename TImage::IndexType centroid = Centroid(iConn, jRegionIds);
  centroid[axis] = j;

  typename TImage::RegionType reg3;
  typename TImage::SizeType   size;
  size.Fill(3);
  size[axis] = 1;
  reg3.SetSize(size);

  typename TImage::IndexType phIndex;
  for (unsigned d = 0; d < TImage::ImageDimension; d++)
  {
    phIndex[d] = centroid.GetIndex()[d] - 1;
  }
  phIndex[axis] = j;
  reg3.SetIndex(phIndex);

  // create a phantom small slice centered around centroid
  typename TImage::Pointer phSlice = TImage::New();
  phSlice->CopyInformation(iConn);
  phSlice->SetRegions(reg3);
  phSlice->Allocate(true);

  // add a phantom point to the center of a newly constructed slice
  phSlice->SetPixel(centroid, 1);
  jRegionIds.clear();
  jRegionIds.push_back(1);
  // WriteDebug(iConn, "C:\\iConn.nrrd");
  // WriteDebug(phSlice, "C:\\phSlice.nrrd");
  typename TImage::IndexType translation = Align(axis, iConn, iRegionId, phSlice, jRegionIds);

  // now translate the phantom slice for best alignment
  for (unsigned d = 0; d < TImage::ImageDimension; d++)
  {
    phIndex[d] -= translation[d];
  }
  phIndex[axis] = j;
  reg3.SetIndex(phIndex);
  phSlice->SetRegions(reg3);
  typename TImage::IndexType t0 = { 0 };
  t0[axis] = j - i;
  Interpolate1to1(axis, out, label, i, j, iConn, iRegionId, phSlice, 1, t0, false);
}


template <typename TImage>
typename MorphologicalContourInterpolator<TImage>::BoolImageType::Pointer
MorphologicalContourInterpolator<TImage>::Dilate1(typename BoolImageType::Pointer seed,
                                                  typename BoolImageType::Pointer mask,
                                                  int                             axis)
{
  thread_local typename DilateType::Pointer m_Dilator = DilateType::New();
  m_Dilator->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  // set up structuring element for dilation
  typedef Size<TImage::ImageDimension> SizeType;
  SizeType                             size;
  size.Fill(1);
  size[axis] = 0;
  thread_local StructuringElementType m_StructuringElement;
  m_StructuringElement.SetRadius(size);
  m_StructuringElement.CreateStructuringElement();
  m_Dilator->SetKernel(m_StructuringElement);
  // TODO: keep structuring element and dilator per axis instead of per thread?

  thread_local typename AndFilterType::Pointer m_And = AndFilterType::New();
  m_And->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  m_Dilator->SetInput(seed);
  m_Dilator->GetOutput()->SetRegions(seed->GetRequestedRegion());
  m_Dilator->Update();
  typename BoolImageType::Pointer temp = m_Dilator->GetOutput();
  temp->DisconnectPipeline();
  // temp->SetRegions(mask->GetLargestPossibleRegion()); //not needed when seed and mask have same regions
  m_And->SetInput(0, mask);
  m_And->SetInput(1, temp);
  m_And->GetOutput()->SetRegions(seed->GetRequestedRegion());
  m_And->Update();
  typename BoolImageType::Pointer result = m_And->GetOutput();
  result->DisconnectPipeline();
  // WriteDebug(seed, "C:\\seed.nrrd");
  // WriteDebug(mask, "C:\\mask.nrrd");
  // WriteDebug(temp, "C:\\temp.nrrd");
  // WriteDebug(result, "C:\\result.nrrd");
  return result;
}


template <typename TImage>
std::vector<typename MorphologicalContourInterpolator<TImage>::BoolImageType::Pointer>
MorphologicalContourInterpolator<TImage>::GenerateDilationSequence(typename BoolImageType::Pointer begin,
                                                                   typename BoolImageType::Pointer end,
                                                                   int                             axis)
{
  // typedef Testing::HashImageFilter<BoolImageType> HashType;
  // HashType::Pointer hasher = HashType::New();
  // hasher->SetInPlace(true);
  // std::vector<std::string> hashes;
  // TODO: optimization: replace ImagesEqual call with hash comparison?
  std::vector<typename BoolImageType::Pointer> seq;
  seq.push_back(Dilate1(begin, end, axis));
  do
  {
    seq.back()->DisconnectPipeline();
    seq.push_back(Dilate1(seq.back(), end, axis));
  } while (!ImagesEqual<BoolImageType>(seq.back(), seq[seq.size() - 2]));
  seq.pop_back(); // remove duplicate image
  return seq;
}


template <typename TImage>
typename MorphologicalContourInterpolator<TImage>::BoolImageType::Pointer
MorphologicalContourInterpolator<TImage>::FindMedianImageDilations(int                             axis,
                                                                   typename BoolImageType::Pointer iMask,
                                                                   typename BoolImageType::Pointer jMask)
{
  // generate sequence
  thread_local typename AndFilterType::Pointer m_And = AndFilterType::New();
  m_And->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  WriteDebug(iMask, "C:\\iMask.nrrd");
  WriteDebug(jMask, "C:\\jMask.nrrd");
  m_And->SetInput(0, iMask);
  m_And->SetInput(1, jMask);
  m_And->GetOutput()->SetRegions(iMask->GetRequestedRegion());
  m_And->Update();
  typename BoolImageType::Pointer intersection = m_And->GetOutput();
  intersection->DisconnectPipeline();
  WriteDebug(intersection, "C:\\intersection.nrrd");
  std::vector<typename BoolImageType::Pointer> iSeq = GenerateDilationSequence(intersection, iMask, axis);
  std::vector<typename BoolImageType::Pointer> jSeq = GenerateDilationSequence(intersection, jMask, axis);
  std::reverse(iSeq.begin(), iSeq.end()); // we want to start from i and end at intersection
  if (iSeq.size() < jSeq.size())
  {
    iSeq.swap(jSeq); // swap so iSeq.size() >= jSeq.size()
  }
  float                                 ratio = float(jSeq.size()) / iSeq.size();
  thread_local typename OrType::Pointer m_Or = OrType::New();
  m_Or->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  std::vector<typename BoolImageType::Pointer> seq;
  for (unsigned x = 0; x < iSeq.size(); x++)
  {
    m_Or->SetInput(0, iSeq[x]);
    unsigned xj = ratio * x;
    m_Or->SetInput(1, jSeq[xj]);
#ifdef _DEBUG
    WriteDebug(iSeq[x], (std::string("C:\\iSeq") + std::to_string(x) + ".nrrd").c_str());
    WriteDebug(jSeq[xj], (std::string("C:\\jSeq") + std::to_string(x) + ".nrrd").c_str());
#endif // _DEBUG
    m_Or->GetOutput()->SetRegions(iMask->GetRequestedRegion());
    m_Or->Update();
    seq.push_back(m_Or->GetOutput());
    seq.back()->DisconnectPipeline();
  }

  // find median
  unsigned       minIndex;
  IdentifierType min = iMask->GetRequestedRegion().GetNumberOfPixels();
  for (unsigned x = 0; x < iSeq.size(); x++)
  {
#ifdef _DEBUG
    WriteDebug(seq[x], (std::string("C:\\seq") + std::to_string(x) + ".nrrd").c_str());
#endif // _DEBUG
    IdentifierType iS = CardSymDifference(seq[x], iMask);
    IdentifierType jS = CardSymDifference(seq[x], jMask);
    IdentifierType xScore = iS >= jS ? iS - jS : jS - iS; // abs(iS-jS)
    if (xScore < min)
    {
      min = xScore;
      minIndex = x;
    }
  }
  return seq[minIndex];
}


template <typename TImage>
typename MorphologicalContourInterpolator<TImage>::FloatImageType::Pointer
MorphologicalContourInterpolator<TImage>::MaurerDM(typename BoolImageType::Pointer mask)
{
  typedef itk::SignedMaurerDistanceMapImageFilter<BoolImageType, FloatImageType> FilterType;
  thread_local typename FilterType::Pointer                                      filter = FilterType::New();
  filter->SetInput(mask);
  filter->SetUseImageSpacing(false); // interpolation algorithm calls for working in index space
  filter->SetNumberOfThreads(1);     // otherwise conflicts with C++11 threads
  filter->GetOutput()->SetRequestedRegion(mask->GetRequestedRegion());
  filter->Update();
  return filter->GetOutput();
}

template <typename TImage>
typename MorphologicalContourInterpolator<TImage>::BoolImageType::Pointer
MorphologicalContourInterpolator<TImage>::FindMedianImageDistances(int                             axis,
                                                                   typename BoolImageType::Pointer iMask,
                                                                   typename BoolImageType::Pointer jMask)
{
  // create intersection
  thread_local typename AndFilterType::Pointer m_And = AndFilterType::New();
  m_And->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  WriteDebug(iMask, "C:\\iMask.nrrd");
  WriteDebug(jMask, "C:\\jMask.nrrd");
  m_And->SetInput(0, iMask);
  m_And->SetInput(1, jMask);
  m_And->GetOutput()->SetRegions(iMask->GetRequestedRegion());
  m_And->Update();
  typename BoolImageType::Pointer intersection = m_And->GetOutput();
  intersection->DisconnectPipeline();
  WriteDebug(intersection, "C:\\intersection.nrrd");

  // calculate distance field
  typename FloatImageType::Pointer sdf = MaurerDM(intersection);
  WriteDebug(sdf, "C:\\sdf.nrrd");

  // create histograms of distances and union
  typename BoolImageType::Pointer orImage = BoolImageType::New();
  orImage->CopyInformation(intersection);
  orImage->SetRegions(iMask->GetRequestedRegion());
  orImage->Allocate(true);
  std::vector<long long>                   iHist;
  std::vector<long long>                   jHist;
  ImageRegionConstIterator<BoolImageType>  iti(iMask, iMask->GetRequestedRegion());
  ImageRegionConstIterator<BoolImageType>  itj(jMask, iMask->GetRequestedRegion());
  ImageRegionIterator<BoolImageType>       ito(orImage, iMask->GetRequestedRegion());
  ImageRegionConstIterator<FloatImageType> itsdf(sdf, iMask->GetRequestedRegion());
  while (!itsdf.IsAtEnd())
  {
    ;
    bool                       iM = iti.Get();
    bool                       jM = itj.Get();
    typename TImage::PixelType dist = itsdf.Get();
    if (iM && !jM)
    {
      if (dist >= iHist.size())
      {
        iHist.resize(dist + 1, 0);
      }
      iHist[dist]++;
      ito.Set(true);
    }
    else if (jM && !iM)
    {
      if (dist >= jHist.size())
      {
        jHist.resize(dist + 1, 0);
      }
      jHist[dist]++;
      ito.Set(true);
    }
    else if (iM && jM)
    {
      ito.Set(true);
    }

    ++iti;
    ++itj;
    ++ito;
    ++itsdf;
  }
  WriteDebug(orImage, "C:\\orImage.nrrd");

  // sum of histogram bins for i and j and
  auto maxSize = std::max(iHist.size(), jHist.size());
  if (maxSize == 0)
  {
    return intersection;
  }
  iHist.resize(maxSize, 0);
  jHist.resize(maxSize, 0);
  assert(iHist[0] == 0);
  assert(jHist[0] == 0);
  std::vector<long long> iSum(maxSize, 0);
  std::vector<long long> jSum(maxSize, 0);
  iSum[0] = iHist[0];
  jSum[0] = jHist[0];
  for (int b = 1; b < maxSize; b++)
  {
    iSum[b] = iSum[b - 1] + iHist[b];
    jSum[b] = jSum[b - 1] + jHist[b];
  }
  long long iTotal = iSum[maxSize - 1];
  long long jTotal = jSum[maxSize - 1];

  // find minimum of differences of sums
  int       bestBin = 0;
  long long bestDiff = LLONG_MAX;
  for (int b = 0; b < maxSize; b++)
  {
    long long iS = std::abs(iTotal - iSum[b] + jSum[b]);
    long long jS = std::abs(jTotal - jSum[b] + iSum[b]);
    // long long jS = std::abs(jTotal - jSum[maxSize - b - 1] + iSum[b]);
    long long diff = std::abs(iS - jS);
    if (diff < bestDiff)
    {
      bestDiff = diff;
      bestBin = b;
    }
  }

  // threshold at distance bestBin is the median intersection
  typedef BinaryThresholdImageFilter<FloatImageType, BoolImageType> FloatBinarizerType;
  thread_local FloatBinarizerType::Pointer                          threshold = FloatBinarizerType::New();
  threshold->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  threshold->SetInput(sdf);
  threshold->SetUpperThreshold(bestBin);
  threshold->GetOutput()->SetRequestedRegion(sdf->GetRequestedRegion());
  threshold->Update();

  thread_local AndFilterType::Pointer and = AndFilterType::New();
  and->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  and->SetInput(threshold->GetOutput());
  and->SetInput(1, orImage);
  and->GetOutput()->SetRequestedRegion(orImage->GetRequestedRegion());
  and->Update();
  BoolImageType::Pointer median = and->GetOutput();
  WriteDebug(median, "C:\\median.nrrd");
  return median;
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::Interpolate1to1(int                             axis,
                                                          TImage *                        out,
                                                          typename TImage::PixelType      label,
                                                          typename TImage::IndexValueType i,
                                                          typename TImage::IndexValueType j,
                                                          typename TImage::Pointer        iConn,
                                                          typename TImage::PixelType      iRegionId,
                                                          typename TImage::Pointer        jConn,
                                                          typename TImage::PixelType      jRegionId,
                                                          typename TImage::IndexType      translation,
                                                          bool                            recursive)
{
  // translate iConn by t/2 and jConn by -t/2
  typename TImage::IndexType  iTrans;
  typename TImage::IndexType  jTrans;
  typename TImage::RegionType iRegion = iConn->GetLargestPossibleRegion();
  typename TImage::RegionType jRegion = jConn->GetLargestPossibleRegion();
  typename TImage::IndexType  jBottom = jRegion.GetIndex();
  bool                        carry = false;

  for (unsigned d = 0; d < TImage::ImageDimension; d++)
  {
    if (!carry)
    {
      iTrans[d] = translation[d] / 2;
      carry = translation[d] % 2;
    }
    else if (translation[d] % 2 == 0)
    {
      iTrans[d] = translation[d] / 2;
    }
    else // use carry
    {
      if (translation[d] > 0)
      {
        iTrans[d] = translation[d] / 2 + 1;
      }
      else
      {
        iTrans[d] = translation[d] / 2 - 1;
      }
      carry = false;
    }
    jTrans[d] = iTrans[d] - translation[d];
    iRegion.SetIndex(d, iRegion.GetIndex()[d] + iTrans[d]);
    jRegion.SetIndex(d, jRegion.GetIndex()[d] + jTrans[d]);
    jBottom[d] = jRegion.GetIndex()[d] + jRegion.GetSize(d) - 1;
  }
  typename TImage::RegionType newRegion = iRegion;
  ExpandRegion(newRegion, jRegion.GetIndex());
  ExpandRegion(newRegion, jBottom);

  WriteDebug(iConn, "C:\\iConn.nrrd");
  WriteDebug(jConn, "C:\\jConn.nrrd");
  typename TImage::Pointer iConnT = TranslateImage(iConn, iTrans, newRegion);
  typename TImage::Pointer jConnT = TranslateImage(jConn, jTrans, newRegion);
  WriteDebug(iConnT, "C:\\iConnT.nrrd");
  WriteDebug(jConnT, "C:\\jConnT.nrrd");

  if (!recursive) // reduce bounding box so we deal with less pixels
  {
    typename TImage::IndexType                minInd = newRegion.GetIndex() + newRegion.GetSize();
    typename TImage::IndexType                maxInd = newRegion.GetIndex();
    ImageRegionConstIteratorWithIndex<TImage> iIt(iConnT, newRegion);
    ImageRegionConstIterator<TImage>          jIt(jConnT, newRegion);

    while (!iIt.IsAtEnd())
    {
      if (iIt.Get() || jIt.Get())
      {
        typename TImage::IndexType ind = iIt.GetIndex();
        for (int d = 0; d < TImage::ImageDimension; d++)
        {
          if (ind[d] < minInd[d])
          {
            minInd[d] = ind[d];
          }
          if (ind[d] > maxInd[d])
          {
            maxInd[d] = ind[d];
          }
        }
      }
      ++iIt;
      ++jIt;
    }

    newRegion.SetIndex(minInd);
    for (int d = 0; d < TImage::ImageDimension; d++)
    {
      newRegion.SetSize(d, maxInd[d] - minInd[d] + 1);
    }
    typename TImage::IndexType t0;
    t0.Fill(0);
    iConnT = TranslateImage(iConnT, t0, newRegion);
    jConnT = TranslateImage(jConnT, t0, newRegion);
  }
  WriteDebug(iConnT, "C:\\iConnTb.nrrd");
  WriteDebug(jConnT, "C:\\jConnTb.nrrd");

  // convert to binary masks
  // TODO: do this by iterators and reduce dimension by one
  MatchesID                                                         matchesIDi(iRegionId);
  MatchesID                                                         matchesIDj(jRegionId);
  typedef UnaryFunctorImageFilter<TImage, BoolImageType, MatchesID> CastType;
  typename CastType::Pointer                                        caster = CastType::New();
  caster->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  caster->SetFunctor(matchesIDi);
  caster->SetInput(iConnT);
  caster->Update();
  typename BoolImageType::Pointer iMask = caster->GetOutput();
  iMask->DisconnectPipeline();
  caster->SetFunctor(matchesIDj);
  caster->SetInput(jConnT);
  caster->Update();
  typename BoolImageType::Pointer jMask = caster->GetOutput();
  jMask->DisconnectPipeline();

  typename BoolImageType::Pointer median;
  if (m_UseDistanceTransform)
  {
    median = FindMedianImageDistances(axis, iMask, jMask);
  }
  else
  {
    median = FindMedianImageDilations(axis, iMask, jMask);
  }

  // finally write it out into the output image pointer
  typename TImage::RegionType outRegion = m_Input->GetLargestPossibleRegion();
  typename TImage::IndexType  t0 = { 0 };
  IntersectionRegions(t0, outRegion, newRegion);
  ImageRegionConstIterator<BoolImageType> seqIt(median, newRegion);
  ImageRegionIterator<TImage>             outIt(out, newRegion);
  while (!outIt.IsAtEnd())
  {
    if (seqIt.Get())
    {
      outIt.Set(label);
    }
    ++outIt;
    ++seqIt;
  }

  // recurse if needed
  typename TImage::IndexValueType mid = newRegion.GetIndex()[axis];
  if (abs(i - j) > 2)
  {
    typedef CastImageFilter<BoolImageType, TImage> InvertCastType;
    typename InvertCastType::Pointer               invCaster = InvertCastType::New();
    invCaster->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
    invCaster->SetInput(median);
    invCaster->Update();
    typename TImage::Pointer midConn = invCaster->GetOutput();
    // midConn->DisconnectPipeline(); //not needed?

    // WriteDebug(midConn, "C:\\midConn.nrrd");
    PixelList regionIDs;
    regionIDs.push_back(1);

    bool first = abs(i - mid) > 1;  // interpolate i-mid?
    bool second = abs(j - mid) > 1; // interpolate j-mid?

    if (first && second && !m_StopSpawning) // then first in new thread
    {
      m_ThreadPool->enqueue(&MorphologicalContourInterpolator<TImage>::Interpolate1to1,
                            this,
                            axis,
                            out,
                            label,
                            i,
                            mid,
                            iConn,
                            iRegionId,
                            midConn,
                            1,
                            iTrans,
                            true);
      ////now give this thread a chance to start by yielding
      // auto oneMS = std::chrono::microseconds(1000);
      // std::this_thread::sleep_for(oneMS);
      ////now continue by recursion in this thread
      Interpolate1to1(axis, out, label, j, mid, jConn, jRegionId, midConn, 1, jTrans, true);
    }
    else // sequential
    {
      if (first)
      {
        Interpolate1to1(axis, out, label, i, mid, iConn, iRegionId, midConn, 1, iTrans, true);
      }
      if (second)
      {
        Interpolate1to1(axis, out, label, j, mid, jConn, jRegionId, midConn, 1, jTrans, true);
      }
    }
  }
  WriteDebug<TImage>(out, "C:\\intermediateResult.nrrd");
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::Interpolate1toN(int                             axis,
                                                          TImage *                        out,
                                                          typename TImage::PixelType      label,
                                                          typename TImage::IndexValueType i,
                                                          typename TImage::IndexValueType j,
                                                          typename TImage::Pointer        iConn,
                                                          typename TImage::PixelType      iRegionId,
                                                          typename TImage::Pointer        jConn,
                                                          PixelList                       jRegionIds,
                                                          typename TImage::IndexType      translation)
{
  // first convert iConn into binary mask
  MatchesID matchesID(iRegionId);

  typedef UnaryFunctorImageFilter<TImage, BoolImageType, MatchesID> CastType;
  typename CastType::Pointer                                        caster = CastType::New();
  caster->SetNumberOfThreads(1); // otherwise conflicts with C++11 threads
  caster->SetFunctor(matchesID);
  caster->SetInput(iConn);
  caster->Update();
  typename BoolImageType::Pointer mask = caster->GetOutput();
  WriteDebug(mask, "C:\\mask.nrrd");
  WriteDebug(iConn, "C:\\iConn.nrrd");
  WriteDebug(jConn, "C:\\jConn.nrrd");

  typename TImage::RegionType iRegion, jRegion, newjRegion;
  iRegion = iConn->GetLargestPossibleRegion();
  jRegion = jConn->GetLargestPossibleRegion();
  newjRegion = jRegion;
  newjRegion.SetSize(iRegion.GetSize());

  // construct n empty images
  std::vector<typename BoolImageType::Pointer> blobs;
  for (unsigned x = 0; x < jRegionIds.size(); x++)
  {
    typename BoolImageType::Pointer temp = BoolImageType::New();
    temp->CopyInformation(jConn);
    temp->SetRegions(iRegion);
    temp->Allocate(true);
    blobs.push_back(temp);
  }

  // fill the n images with intersections - these are seeds
  typename TImage::Pointer belongs = TImage::New();
  belongs->CopyInformation(mask);
  belongs->SetRegions(iRegion);
  belongs->Allocate(true); // initialize to zero (false)
  ImageRegionIterator<TImage> belongIt(belongs, iRegion);
  IntersectionRegions(translation, iRegion, jRegion);
  ImageRegionConstIterator<BoolImageType>   maskIt(mask, iRegion);
  ImageRegionConstIteratorWithIndex<TImage> jIt(jConn, jRegion);
  ImageRegionIterator<TImage>               belongInit(belongs, iRegion);

  // convert jConn into n blobs, translating them into the index space of iConn
  while (!maskIt.IsAtEnd())
  {
    if (maskIt.Get())
    {
      typename TImage::PixelType   jVal = jIt.Get();
      typename PixelList::iterator res = std::find(jRegionIds.begin(), jRegionIds.end(), jVal);
      if (res != jRegionIds.end())
      {
        blobs[res - jRegionIds.begin()]->SetPixel(maskIt.GetIndex(), true);
        belongInit.Set(res - jRegionIds.begin() + 1);
      }
    }
    ++maskIt;
    ++jIt;
    ++belongInit;
  }
  WriteDebug(belongs, "C:\\belongs.nrrd");

  // prepare dilation filter
  iRegion = iConn->GetLargestPossibleRegion(); // expand to full i image
  for (unsigned x = 0; x < jRegionIds.size(); x++)
  {
    blobs[x]->SetRegions(iRegion);
    WriteDebug(blobs[x], (std::string("C:\\blob") + char('0' + x) + ".nrrd").c_str());
  }
  ImageRegionConstIterator<BoolImageType>          maskIt2(mask, iRegion);
  ImageRegionConstIteratorWithIndex<BoolImageType> jIt2(blobs[0], iRegion);

  bool hollowedMaskEmpty;
  do // while hollowed mask is not empty
  {
    for (unsigned x = 0; x < jRegionIds.size(); x++)
    {
      blobs[x] = Dilate1(blobs[x], mask, axis);
      WriteDebug(blobs[x], (std::string("C:\\blob") + char('0' + x) + ".nrrd").c_str());
      blobs[x]->DisconnectPipeline();
      // TODO: save these in a sequence so we don't have to recalculate it!
    }

    // TODO: replace this loop by LabelErodeDilate filters?
    hollowedMaskEmpty = true;
    maskIt2.GoToBegin();
    jIt2.GoToBegin();
    belongIt.GoToBegin();
    while (!maskIt2.IsAtEnd()) // hollow out the big mask with dilated seeds while avoiding conflicts
    {
      if (maskIt2.Get())
      {
        if (!belongIt.Get())
        {
          unsigned x = 0;
          for (; x < jRegionIds.size(); x++)
          {
            if (blobs[x]->GetPixel(jIt2.GetIndex()))
            {
              break;
            }
          }
          if (x < jRegionIds.size()) // covered by a blob, hollow it out
          {
            belongIt.Set(x + 1);
            for (x++; x < jRegionIds.size(); x++)
            {
              // pixel does not belong to this blob
              blobs[x]->SetPixel(jIt2.GetIndex(), false);
            }
          }
          else // keep it
          {
            hollowedMaskEmpty = false;
          }
        }
        else // the pixel already belongs to some blob
        {
          for (unsigned x = 0; x < jRegionIds.size(); x++)
          {
            if (belongIt.Get() != x + 1)
            {
              // pixel does not belong to this blob
              blobs[x]->SetPixel(jIt2.GetIndex(), false);
            }
          }
        }
      }
      ++maskIt2;
      ++jIt2;
      ++belongIt;
    }
    WriteDebug(belongs, "C:\\belongs.nrrd");
  } while (!hollowedMaskEmpty);
  blobs.clear(); // deallocates the images

  // convert the belongs into n Conn-style images
  std::vector<typename TImage::Pointer> conns;
  for (unsigned x = 0; x < jRegionIds.size(); x++)
  {
    typename TImage::Pointer temp2 = TImage::New();
    temp2->CopyInformation(iConn);
    temp2->SetRegions(iConn->GetLargestPossibleRegion());
    temp2->Allocate(true);
    conns.push_back(temp2);
  }
  ImageRegionConstIteratorWithIndex<TImage> belongIt2(belongs, iRegion);
  while (!belongIt2.IsAtEnd())
  {
    const typename TImage::PixelType belong = belongIt2.Get();
    if (belong > 0)
    {
      conns[belong - 1]->SetPixel(belongIt2.GetIndex(), iRegionId);
    }
    ++belongIt2;
  }

  // make n 1-to-1 interpolations
  for (unsigned x = 0; x < jRegionIds.size(); x++)
  {
    Interpolate1to1(axis, out, label, i, j, conns[x], iRegionId, jConn, jRegionIds[x], translation, false);
    // TODO: call sequence construction directly from here!
  }
}


template <typename TImage>
typename TImage::Pointer
MorphologicalContourInterpolator<TImage>::TranslateImage(typename TImage::Pointer    image,
                                                         typename TImage::IndexType  translation,
                                                         typename TImage::RegionType newRegion)
{
  typename TImage::Pointer result = TImage::New();
  result->CopyInformation(image);
  result->SetRegions(newRegion);
  result->Allocate(true); // initialize to zero (false)
  typename TImage::RegionType inRegion = image->GetLargestPossibleRegion();
  IntersectionRegions(translation, inRegion, newRegion);
  ImageAlgorithm::Copy<TImage, TImage>(image.GetPointer(), result.GetPointer(), inRegion, newRegion);
  return result;
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::IntersectionRegions(typename TImage::IndexType    translation,
                                                              typename TImage::RegionType & iRegion,
                                                              typename TImage::RegionType & jRegion)
{
  typename TImage::IndexType iBegin = iRegion.GetIndex();
  typename TImage::IndexType jBegin = jRegion.GetIndex();
  for (IdentifierType d = 0; d < TImage::ImageDimension; d++)
  {
    IndexValueType iSize = iRegion.GetSize(d);
    IndexValueType jSize = jRegion.GetSize(d);
    iBegin[d] += translation[d];
    IndexValueType t = std::max(iBegin[d], jBegin[d]);
    iRegion.SetSize(d, std::min(IndexValueType(iSize) - (t - iBegin[d]), IndexValueType(jSize) - (t - jBegin[d])));
    iRegion.SetIndex(d, t - translation[d]);
    jRegion.SetIndex(d, t);
  }
  jRegion.SetSize(iRegion.GetSize()); // size is the same
}


template <typename TImage>
IdentifierType
MorphologicalContourInterpolator<TImage>::Intersection(typename TImage::Pointer   iConn,
                                                       typename TImage::PixelType iRegionId,
                                                       typename TImage::Pointer   jConn,
                                                       PixelList                  jRegionIds,
                                                       typename TImage::IndexType translation)
{
  typename TImage::RegionType iRegion, jRegion;
  iRegion = iConn->GetLargestPossibleRegion();
  jRegion = jConn->GetLargestPossibleRegion();
  IntersectionRegions(translation, iRegion, jRegion);

  std::vector<IdentifierType> counts(jRegionIds.size());
  for (int x = 0; x < jRegionIds.size(); x++)
  {
    counts[x] = 0;
  }
  ImageRegionConstIterator<TImage> iIt(iConn, iRegion);
  ImageRegionConstIterator<TImage> jIt(jConn, jRegion);
  while (!iIt.IsAtEnd())
  {
    if (iIt.Get() == iRegionId)
    {
      typename TImage::PixelType   jVal = jIt.Get();
      typename PixelList::iterator res = std::find(jRegionIds.begin(), jRegionIds.end(), jVal);
      if (res != jRegionIds.end())
      {
        ++counts[res - jRegionIds.begin()];
      }
    }
    ++iIt;
    ++jIt;
  }
  IdentifierType sum = 0;
  for (int x = 0; x < jRegionIds.size(); x++)
  {
    if (counts[x] == 0)
    {
      return 0; // iConn must intersect all subregions of jConn
    }
    sum += counts[x];
  }
  return sum;
}

template <typename TImage>
IdentifierType
MorphologicalContourInterpolator<TImage>::CardSymDifference(
  typename MorphologicalContourInterpolator<TImage>::BoolImageType::Pointer iShape,
  typename MorphologicalContourInterpolator<TImage>::BoolImageType::Pointer jShape)
{
  typename TImage::RegionType             region = iShape->GetLargestPossibleRegion();
  IdentifierType                          count = 0;
  ImageRegionConstIterator<BoolImageType> iIt(iShape, region);
  ImageRegionConstIterator<BoolImageType> jIt(jShape, region);
  while (!iIt.IsAtEnd())
  {
    if (iIt.Get() != jIt.Get())
    {
      count++;
    }
    ++iIt;
    ++jIt;
  }
  return count;
}

template <typename TImage>
typename TImage::IndexType
MorphologicalContourInterpolator<TImage>::Centroid(typename TImage::Pointer conn, PixelList regionIds)
{
  ImageRegionConstIteratorWithIndex<TImage> it(conn, conn->GetLargestPossibleRegion());
  IndexValueType ind[TImage::ImageDimension] = { 0 }; // all components are initialized to zero
  IdentifierType pixelCount = 0;
  while (!it.IsAtEnd())
  {
    typename TImage::PixelType val = it.Get();
    if (val)
    {
      typename PixelList::iterator res = std::find(regionIds.begin(), regionIds.end(), val);
      if (res != regionIds.end())
      {
        ++pixelCount;
        typename TImage::IndexType pInd = it.GetIndex();
        for (unsigned d = 0; d < TImage::ImageDimension; d++)
        {
          ind[d] += pInd[d];
        }
      }
    }
    ++it;
  }
  typename TImage::IndexType retVal;
  for (unsigned d = 0; d < TImage::ImageDimension; d++)
  {
    retVal[d] = ind[d] / pixelCount;
  }
  return retVal;
}


template <typename TImage>
typename TImage::IndexType
MorphologicalContourInterpolator<TImage>::Align(int                        axis,
                                                typename TImage::Pointer   iConn,
                                                typename TImage::PixelType iRegionId,
                                                typename TImage::Pointer   jConn,
                                                PixelList                  jRegionIds)
{
  // calculate centroids
  PixelList iRegionIds;
  iRegionIds.push_back(iRegionId);
  typename TImage::IndexType iCentroid = Centroid(iConn, iRegionIds);
  typename TImage::IndexType jCentroid = Centroid(jConn, jRegionIds);

  typename TImage::IndexType ind;
  for (unsigned d = 0; d < TImage::ImageDimension; d++)
  {
    ind[d] = jCentroid[d] - iCentroid[d];
  }

  // construct an image with all possible translations
  typename TImage::RegionType searchRegion;
  typename TImage::RegionType iLPR = iConn->GetLargestPossibleRegion();
  typename TImage::RegionType jLPR = jConn->GetLargestPossibleRegion();
  for (IdentifierType d = 0; d < TImage::ImageDimension; d++)
  {
    searchRegion.SetIndex(d, jLPR.GetIndex()[d] - iLPR.GetIndex()[d] - iLPR.GetSize(d) + 1);
    searchRegion.SetSize(d, iLPR.GetSize(d) + jLPR.GetSize(d) - 1);
  }
  // searchRegion.SetSize(axis, 1);
  // searchRegion.SetIndex(axis, 0);

  typename BoolImageType::Pointer searched = BoolImageType::New();
  searched->SetRegions(searchRegion);
  searched->Allocate(true); // initialize to zero (false)

  // breadth first search starting from centroid, implicitly:
  // when intersection scores are equal, chooses the one closer to centroid
  std::queue<typename TImage::IndexType> uncomputed;
  typename TImage::IndexType             t0 = { 0 };
  t0[axis] = ind[axis];
  uncomputed.push(t0);  // no translation - guaranteed to find a non-zero intersection
  uncomputed.push(ind); // this introduces movement, and possibly has the same score
  searched->SetPixel(t0, true);
  searched->SetPixel(ind, true);
  IdentifierType             score, maxScore = 0;
  typename TImage::IndexType bestIndex;
  IdentifierType             iter = 0;
  IdentifierType             minIter = std::min(m_MinAlignIters, searchRegion.GetNumberOfPixels());
  IdentifierType maxIter = std::max(m_MaxAlignIters, (IdentifierType)sqrt(searchRegion.GetNumberOfPixels()));

  // debug: construct and later fill the image with intersection scores
#ifndef NDEBUG
  typename TImage::Pointer scoreImage = TImage::New();
  scoreImage->SetRegions(searchRegion);
  scoreImage->Allocate(true);
#endif // NDEBUG

  while (!uncomputed.empty())
  {
    ind = uncomputed.front();
    uncomputed.pop();
    score = Intersection(iConn, iRegionId, jConn, jRegionIds, ind);
#ifndef NDEBUG
    scoreImage->SetPixel(ind, score + 1); // unexplored=0, noIntersection=1
#endif                                    // NDEBUG

    if (score > maxScore)
    {
      maxScore = score;
      bestIndex = ind;
    }

    // we breadth this search
    if (!m_HeuristicAlignment || maxScore == 0 || iter <= minIter || score > maxScore * 0.9 && iter <= maxIter)
    {
      for (unsigned d = 0; d < TImage::ImageDimension; d++)
      {
        if (d == axis)
        {
          continue; // do not waste time on additional checks
        }
        ind[d] -= 1; //"left"
        if (searchRegion.IsInside(ind) && !searched->GetPixel(ind))
        {
          uncomputed.push(ind);
          searched->SetPixel(ind, true);
          ++iter;
        }
        ind[d] += 2; //"right"
        if (searchRegion.IsInside(ind) && !searched->GetPixel(ind))
        {
          uncomputed.push(ind);
          searched->SetPixel(ind, true);
          ++iter;
        }
        ind[d] -= 1; // return to initial
      }
    }
  }
  // WriteDebug(searched, "C:\\searched.nrrd");
#ifndef NDEBUG
  WriteDebug(scoreImage, "C:\\scoreImage.nrrd");
#endif // NDEBUG
  return bestIndex;
}


template <typename TImage>
typename TImage::Pointer
MorphologicalContourInterpolator<TImage>::RegionedConnectedComponents(const typename TImage::RegionType region,
                                                                      typename TImage::PixelType        label,
                                                                      IdentifierType &                  objectCount)
{
  m_RoI->SetExtractionRegion(region);
  m_RoI->SetInput(m_Input);
  m_Binarizer->SetLowerThreshold(label);
  m_Binarizer->SetUpperThreshold(label);
  m_ConnectedComponents->Update();
  objectCount = m_ConnectedComponents->GetObjectCount();
  return m_ConnectedComponents->GetOutput();
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::InterpolateBetweenTwo(int                             axis,
                                                                TImage *                        out,
                                                                typename TImage::PixelType      label,
                                                                typename TImage::IndexValueType i,
                                                                typename TImage::IndexValueType j,
                                                                typename TImage::Pointer        iconn,
                                                                typename TImage::Pointer        jconn)
{
  // go through comparison image and create correspondence pairs
  typedef std::set<std::pair<typename TImage::PixelType, typename TImage::PixelType>> PairSet;
  PairSet                          pairs, unwantedPairs, uncleanPairs;
  typename TImage::RegionType      ri = iconn->GetRequestedRegion();
  typename TImage::RegionType      rj = jconn->GetRequestedRegion();
  ImageRegionConstIterator<TImage> iti(iconn, ri);
  ImageRegionConstIterator<TImage> itj(jconn, rj);
  while (!iti.IsAtEnd())
  {
    if (iti.Get() != 0 || itj.Get() != 0)
    {
      uncleanPairs.insert(std::make_pair(iti.Get(), itj.Get()));
      // std::cout << " iti:" << iti.GetIndex() << iti.Get() <<
      //   " itj:" << itj.GetIndex() << itj.Get() << std::endl;
      if (iti.Get() != 0 && itj.Get() != 0)
      {
        unwantedPairs.insert(std::make_pair(0, itj.Get()));
        unwantedPairs.insert(std::make_pair(iti.Get(), 0));
      }
    }
    ++iti;
    ++itj;
  }
  std::set_difference(uncleanPairs.begin(),
                      uncleanPairs.end(),
                      unwantedPairs.begin(),
                      unwantedPairs.end(),
                      std::inserter(pairs, pairs.end()));

  // first do extrapolation for components without overlaps
  typename PairSet::iterator p = pairs.begin();
  while (p != pairs.end())
  {
    if (p->second == 0)
    {
      Extrapolate(axis, out, label, i, j, iconn, p->first);
      pairs.erase(p++);
    }
    else if (p->first == 0)
    {
      Extrapolate(axis, out, label, j, i, jconn, p->second);
      pairs.erase(p++);
    }
    else
    {
      ++p;
    }
  }

  // count ocurrances of each component
  typedef std::map<typename TImage::PixelType, IdentifierType> CountMap;
  CountMap                                                     iCounts, jCounts;
  for (p = pairs.begin(); p != pairs.end(); ++p)
  {
    iCounts[p->first]++;
    jCounts[p->second]++;
  }

  // now handle 1 to 1 correspondences
  p = pairs.begin();
  while (p != pairs.end())
  {
    if (iCounts[p->first] == 1 && jCounts[p->second] == 1)
    {
      PixelList regionIDs;
      regionIDs.push_back(p->second);
      typename TImage::IndexType translation = Align(axis, iconn, p->first, jconn, regionIDs);
      Interpolate1to1(axis, out, label, i, j, iconn, p->first, jconn, p->second, translation, false);
      iCounts.erase(p->first);
      jCounts.erase(p->second);
      pairs.erase(p++);
    }
    else
    {
      ++p;
    }
  }

  PixelList regionIDs(pairs.size()); // preallocate
  // now do 1-to-N and M-to-1 cases
  p = pairs.begin();
  while (p != pairs.end())
  {
    regionIDs.clear();

    if (iCounts[p->first] == 1) // M-to-1
    {
      for (typename PairSet::iterator rest = pairs.begin(); rest != pairs.end(); ++rest)
      {
        if (rest->second == p->second)
        {
          regionIDs.push_back(rest->first);
        }
      }

      typename TImage::IndexType translation = Align(axis, jconn, p->second, iconn, regionIDs);
      Interpolate1toN(axis, out, label, j, i, jconn, p->second, iconn, regionIDs, translation);

      typename PairSet::iterator rest = pairs.begin();
      while (rest != pairs.end())
      {
        if (rest != p && rest->second == p->second)
        {
          --iCounts[rest->first];
          --jCounts[rest->second];
          pairs.erase(rest++);
        }
        else
        {
          ++rest;
        }
      }
      --iCounts[p->first];
      --jCounts[p->second];
      pairs.erase(p++);
    } // M-to-1
    else if (jCounts[p->second] == 1) // 1-to-N
    {
      for (typename PairSet::iterator rest = pairs.begin(); rest != pairs.end(); ++rest)
      {
        if (rest->first == p->first)
        {
          regionIDs.push_back(rest->second);
        }
      }

      typename TImage::IndexType translation = Align(axis, iconn, p->first, jconn, regionIDs);
      Interpolate1toN(axis, out, label, i, j, iconn, p->first, jconn, regionIDs, translation);

      typename PairSet::iterator rest = pairs.begin();
      ++rest;
      while (rest != pairs.end())
      {
        if (rest != p && rest->first == p->first)
        {
          --iCounts[rest->first];
          --jCounts[rest->second];
          pairs.erase(rest++);
        }
        else
        {
          ++rest;
        }
      }
      --iCounts[p->first];
      --jCounts[p->second];
      pairs.erase(p++);
    } // 1-to-N
    else
    {
      ++p;
    }
  } // 1-to-N and M-to-1

  // only M-to-N correspondences remain
  // we turn each M-to-N case into m 1-to-N cases
  p = pairs.begin();
  while (p != pairs.end())
  {
    regionIDs.clear();
    for (typename PairSet::iterator rest = p; rest != pairs.end(); ++rest)
    {
      if (rest->first == p->first)
      {
        regionIDs.push_back(rest->second);
      }
    }

    typename TImage::IndexType translation = Align(axis, iconn, p->first, jconn, regionIDs);
    Interpolate1toN(axis, out, label, i, j, iconn, p->first, jconn, regionIDs, translation);

    typename PairSet::iterator rest = p;
    ++rest;
    while (rest != pairs.end())
    {
      if (rest->first == p->first)
      {
        pairs.erase(rest++);
      }
      else
      {
        ++rest;
      }
    }
    // counts no longer matter, do not waste time deleting them
    pairs.erase(p++);
  } // M-to-N
} // void MorphologicalContourInterpolator::InterpolateBetweenTwo()


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::InterpolateAlong(int axis, TImage * out)
{
  // do multithreading by paralellizing for different labels
  // and different inter-slice segments [thread pool of C++11 threads]
  m_ThreadPool = new ::ThreadPool(std::thread::hardware_concurrency());
  std::vector<decltype(m_ThreadPool->enqueue(
    &MorphologicalContourInterpolator<TImage>::InterpolateBetweenTwo, this, axis, out, 0, 0, 0, out, out))>
    results; // so we can wait for all the results
  m_StopSpawning = false;

  for (typename LabeledSlicesType::iterator it = m_LabeledSlices[axis].begin(); it != m_LabeledSlices[axis].end(); ++it)
  {
    if (m_Label == 0 || m_Label == it->first) // label needs to be interpolated
    {
      typename SliceSetType::iterator prev = it->second.begin();
      if (prev == it->second.end())
      {
        continue; // nothing to do for this label
      }

      typename TImage::RegionType ri;
      ri = m_BoundingBoxes[it->first];
      ri.SetSize(axis, 1);
      ri.SetIndex(axis, *prev);
      IdentifierType           xCount;
      typename TImage::Pointer iconn = this->RegionedConnectedComponents(ri, it->first, xCount);
      iconn->DisconnectPipeline();

      typename SliceSetType::iterator next = it->second.begin();
      for (++next; next != it->second.end(); ++next)
      {
        typename TImage::RegionType rj = ri;
        rj.SetIndex(axis, *next);
        typename TImage::Pointer jconn = this->RegionedConnectedComponents(rj, it->first, xCount);
        jconn->DisconnectPipeline();

        WriteDebug(iconn, "C:\\iconn.nrrd");
        WriteDebug(jconn, "C:\\jconn.nrrd");

        if (*prev + 1 < *next) // only if they are not adjacent slices
        {
          results.push_back(m_ThreadPool->enqueue(&MorphologicalContourInterpolator<TImage>::InterpolateBetweenTwo,
                                                  this,
                                                  axis,
                                                  out,
                                                  it->first,
                                                  *prev,
                                                  *next,
                                                  iconn,
                                                  jconn));
        }
        iconn = jconn;
        prev = next;
      }
    }
  }

  for (int i = 0; i < results.size(); i++)
  {
    results[i].get(); // wait for thread
  }
  m_StopSpawning = true;
  // invoking destructor waits for any leftover threads created by Interpolate1to1
  delete m_ThreadPool;
  m_ThreadPool = nullptr;
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::CombineInputAndInterpolate(typename TImage::Pointer interpolate)
{
  ImageRegionIterator<TImage>      itO(m_Output, m_Output->GetBufferedRegion());
  ImageRegionConstIterator<TImage> itI(m_Input, m_Output->GetBufferedRegion());
  ImageRegionConstIterator<TImage> it(interpolate, m_Output->GetBufferedRegion());
  while (!it.IsAtEnd())
  {
    typename TImage::PixelType val = itI.Get();
    if (val != 0)
    {
      itO.Set(val);
    }
    else
    {
      itO.Set(it.Get());
    }

    ++it;
    ++itI;
    ++itO;
  }

  // put the output data back into the regular pipeline
  this->GraftOutput(m_Output);
  this->m_Output = ITK_NULLPTR;
}


template <typename TImage>
void
MorphologicalContourInterpolator<TImage>::GenerateData()
{
  m_Input = TImage::New();
  m_Input->Graft(const_cast<TImage *>(this->GetInput()));
  this->AllocateOutputs();
  m_Output = TImage::New();
  m_Output->Graft(this->GetOutput());
  typename TImage::Pointer tempOut = TImage::New();
  tempOut->CopyInformation(m_Output);
  tempOut->SetRegions(m_Output->GetLargestPossibleRegion());

  this->DetermineSliceOrientations();

  if (m_BoundingBoxes.size() == 0) // empty input image
  {
    tempOut->Allocate(true);
    CombineInputAndInterpolate(tempOut);
    return;
  }

  if (m_Axis == -1) // interpolate along all axes
  {
    OrientationType aggregate = OrientationType();
    aggregate.Fill(false);

    if (this->m_Label == 0)
    {
      for (typename OrientationsType::iterator it = m_Orientations.begin(); it != m_Orientations.end(); ++it)
      {
        for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
        {
          aggregate[a] = aggregate[a] || it->second[a]; // any label needs interpolation along this axis
        }
      }
    }
    else
    {
      aggregate = m_Orientations[m_Label]; // we only care about this label
    }

    std::vector<typename TImage::Pointer> perAxisInterpolates;
    for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
    {
      if (aggregate[a])
      {
        typename TImage::Pointer imageA = TImage::New();
        imageA->CopyInformation(m_Output);
        imageA->SetRegions(m_Output->GetRequestedRegion());
        imageA->Allocate(true);
        this->InterpolateAlong(a, imageA);
        perAxisInterpolates.push_back(imageA);
      }
    }

    if (perAxisInterpolates.size() == 0) // nothing to process
    {
      tempOut->Allocate(true);
      CombineInputAndInterpolate(tempOut);
      return;
    }
    if (perAxisInterpolates.size() == 1)
    {
      CombineInputAndInterpolate(perAxisInterpolates[0]);
      return;
    }
    // else
    std::vector<ImageRegionConstIterator<TImage>> iterators;

    for (int i = 0; i < perAxisInterpolates.size(); ++i)
    {
      ImageRegionConstIterator<TImage> it(perAxisInterpolates[i], m_Output->GetRequestedRegion());
      iterators.push_back(it);
    }

    std::vector<typename TImage::PixelType> values;
    values.reserve(perAxisInterpolates.size());

    tempOut->Allocate(true);
    ImageRegionIterator<TImage> it(tempOut, m_Output->GetRequestedRegion());
    while (!it.IsAtEnd())
    {
      values.clear();
      for (int i = 0; i < perAxisInterpolates.size(); ++i)
      {
        typename TImage::PixelType val = iterators[i].Get();
        if (val != 0)
        {
          it.Set(val); // last written value stays
        }
      }

      // next pixel
      ++it;
      for (int i = 0; i < perAxisInterpolates.size(); ++i)
      {
        ++(iterators[i]);
      }
    }
  } // interpolate along all axes
  else // interpolate along the specified axis
  {
    tempOut->Allocate(true);
    this->InterpolateAlong(m_Axis, tempOut);
  }
  CombineInputAndInterpolate(tempOut);
}

} // end namespace itk

#endif // itkMorphologicalContourInterpolator_hxx
