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
#include <utility>
#include <algorithm>
#include <queue>

// DEBUG
#include <iostream>

namespace itk
{
template <class TImage>
MorphologicalContourInterpolator<TImage>::MorphologicalContourInterpolator()
  : m_Label(0)
  , m_Axis(-1)
  , m_HeuristicAlignment(true)
  , m_LabeledSlices(TImage::ImageDimension) // initialize with empty sets
{
  m_RoI = RoiType::New();
  m_Binarizer = BinarizerType::New();
  m_Binarizer->SetInput(m_RoI->GetOutput());
  m_ConnectedComponents = ConnectedComponentsType::New();
  m_ConnectedComponents->SetInput(m_Binarizer->GetOutput());
  m_ConnectedComponents->SetFullyConnected(true);
}

template <class TImage>
typename MorphologicalContourInterpolator<TImage>::SliceSetType
MorphologicalContourInterpolator<TImage>::GetLabeledSliceIndices(unsigned int axis)
{
  return m_LabeledSlices[axis];
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::SetLabeledSliceIndices(unsigned int axis, SliceSetType indices)
{
  m_LabeledSlices[axis] = indices;
  this->Modified();
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::SetLabeledSliceIndices(unsigned int                                 axis,
                                                                 std::vector<typename TImage::IndexValueType> indices)
{
  m_LabeledSlices[axis] = SliceSetType().insert(indices.begin(), indices.end());
  this->Modified();
}

template <class TImage>
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
    else if (region.GetIndex(a) + region.GetSize(a) <= index[a])
    {
      region.SetSize(a, index[a] - region.GetIndex(a) + 1);
    }
    // else it is already within
  }
}

template <class TImage>
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
    typename const TImage::IndexType ind = it.GetIndex();
    typename const TImage::PixelType val = m_Input->GetPixel(ind);
    if (val != 0 || (m_Label != 0 && val == m_Label))
    {
      typename TImage::RegionType boundingBox1;
      boundingBox1.SetIndex(ind);
      for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
      {
        boundingBox1.SetSize(a, 1);
      }
      std::pair<BoundingBoxesType::iterator, bool> resBB = m_BoundingBoxes.insert(std::make_pair(val, boundingBox1));
      if (!resBB.second) // include this index in existing BB
      {
        ExpandRegion(resBB.first->second, ind);
      }

      std::pair<OrientationsType::iterator, bool> res = m_Orientations.insert(std::make_pair(val, orientations));
      OrientationsType::iterator                  oRef = res.first;
      unsigned int                                cTrue = 0, cAdjacent = 0, axis;
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
        if (prev == 0 && next == 0)
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

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::Extrapolate(int                             axis,
                                                      typename TImage *               out,
                                                      typename TImage::PixelType      label,
                                                      typename TImage::IndexValueType i,
                                                      typename TImage::IndexValueType j,
                                                      typename TImage::Pointer        iConn,
                                                      typename TImage::PixelType      iRegionId)
{}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::Interpolate1to1(int                             axis,
                                                          typename TImage *               out,
                                                          typename TImage::PixelType      label,
                                                          typename TImage::IndexValueType i,
                                                          typename TImage::IndexValueType j,
                                                          typename TImage::Pointer        iConn,
                                                          typename TImage::PixelType      iRegionId,
                                                          typename TImage::Pointer        jConn,
                                                          typename TImage::PixelType      jRegionId)
{
  PixelList jRegionIds;
  jRegionIds.push_back(jRegionId);
  Align(axis, iConn, iRegionId, jConn, jRegionIds);
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::Interpolate1toN(int                             axis,
                                                          typename TImage *               out,
                                                          typename TImage::PixelType      label,
                                                          typename TImage::IndexValueType i,
                                                          typename TImage::IndexValueType j,
                                                          typename TImage::Pointer        iConn,
                                                          typename TImage::PixelType      iRegionId,
                                                          typename TImage::Pointer        jConn,
                                                          PixelList                       jRegionIds)
{
  Align(axis, iConn, iRegionId, jConn, jRegionIds);
}


// not used? delete
template <class TImage>
typename MorphologicalContourInterpolator<TImage>::BoundingBoxesType &
MorphologicalContourInterpolator<TImage>::CalculateBoundingBoxes(typename TImage::Pointer image,
                                                                 const PixelList &        regions)
{
  BoundingBoxesType           bb; // result
  typename TImage::RegionType zeroRegion;
  zeroRegion.GetIndex().Fill(0);
  zeroRegion.GetSize().Fill(0);
  for (int i = 0; i < regions.size(); i++)
  {
    bb[regions[i]] = zeroRegion;
  }

  ImageRegionConstIteratorWithIndex<TImage> it(image, image->GetLargestPossibleRegion());
  for (; !it.IsAtEnd(); ++it)
  {
    typename const TImage::IndexType ind = it.GetIndex();
    typename const TImage::PixelType val = it.Value();
    if (val == 0)
    {
      continue; // next pixel
    }
    BoundingBoxesType::iterator bbi = bb.find(val);
    if (bbi == bb.end() || bbi->second == zeroRegion)
    {
      continue; // next pixel
    }
    ExpandRegion(bbi->second, ind);
  }
  return bb;
}

template <class TImage>
typename TImage::RegionType
MorphologicalContourInterpolator<TImage>::MergeBoundingBoxes(const BoundingBoxesType & boundingBoxes)
{
  BoundingBoxesType::iterator it = m_BoundingBoxes.begin();
  typename TImage::RegionType result = it->second;
  typename TImage::SizeType   minusOne;
  minusOne.Fill(-1);
  for (++it; it != m_BoundingBoxes.end(); ++it)
  {
    ExpandRegion(result, it->second.GetIndex());
    ExpandRegion(result, it->second.GetIndex() + it->second.GetSize() + minusOne);
  }
  return result;
}

template <class TImage>
IdentifierType
MorphologicalContourInterpolator<TImage>::Intersection(typename TImage::Pointer   iConn,
                                                       typename TImage::PixelType iRegionId,
                                                       typename TImage::Pointer   jConn,
                                                       PixelList                  jRegionIds,
                                                       typename TImage::IndexType translation)
{
  typename TImage::RegionType iRegion, jRegion;
  typename TImage::IndexType  iBegin = iConn->GetLargestPossibleRegion().GetIndex();
  typename TImage::IndexType  jBegin = jConn->GetLargestPossibleRegion().GetIndex();
  for (IdentifierType d = 0; d < TImage::ImageDimension; d++)
  {
    IdentifierType iSize = iConn->GetLargestPossibleRegion().GetSize(d);
    IdentifierType jSize = jConn->GetLargestPossibleRegion().GetSize(d);
    IndexValueType t = translation[d];
    if (t >= 0)
    {
      iRegion.SetSize(d, std::min(iSize - t, jSize));
      iRegion.SetIndex(d, iBegin[d] + t);
      jRegion.SetIndex(d, jBegin[d]);
    }
    else
    {
      iRegion.SetSize(d, std::min(iSize, jSize + t));
      iRegion.SetIndex(d, iBegin[d]);
      jRegion.SetIndex(d, jBegin[d] - t);
    }
  }
  jRegion.SetSize(iRegion.GetSize()); // size is the same

  IdentifierType                   count = 0;
  ImageRegionConstIterator<TImage> iIt(iConn, iRegion);
  ImageRegionConstIterator<TImage> jIt(jConn, jRegion);
  while (!iIt.IsAtEnd())
  {
    if (iIt.Get() == iRegionId)
    {
      typename TImage::PixelType jVal = jIt.Get();
      PixelList::iterator        res = std::find(jRegionIds.begin(), jRegionIds.end(), jVal);
      if (res != jRegionIds.end())
      {
        count++;
      }
    }
    ++iIt;
    ++jIt;
  }
  return count;
}

// unused
template <class TImage>
IdentifierType
MorphologicalContourInterpolator<TImage>::Distance2(typename TImage::IndexType t1, typename TImage::IndexType t2)
{
  IdentifierType dist = 0;
  for (unsigned d = 0; d < TImage::ImageDimension; d++)
  {
    dist += (t1[d] - t2[d]) * (t1[d] - t2[d]);
  }
  return dist;
}

template <class TImage>
typename TImage::IndexType
MorphologicalContourInterpolator<TImage>::Centroid(typename TImage::Pointer conn, PixelList regionIds)
{
  ImageRegionConstIteratorWithIndex<TImage> it(conn, conn->GetLargestPossibleRegion());
  IndexValueType ind[TImage::ImageDimension] = { 0 }; // all components are initialized to zero
  IdentifierType pixelCount = 0;
  while (!it.IsAtEnd())
  {
    typename TImage::PixelType val = it.Get();
    PixelList::iterator        res = std::find(regionIds.begin(), regionIds.end(), val);
    if (res != regionIds.end())
    {
      ++pixelCount;
      typename TImage::IndexType pInd = it.GetIndex();
      for (unsigned d = 0; d < TImage::ImageDimension; d++)
      {
        ind[d] += pInd[d];
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

template <class TImage>
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

  typename TImage::IndexType ind, centroidInd;
  for (unsigned d = 0; d < TImage::ImageDimension; d++)
  {
    ind[d] = iCentroid[d] - jCentroid[d];
  }
  ind[axis] = 0; // i and j have different coordinate along this axis
  centroidInd = ind;

  // construct an image with all possible translations
  typename TImage::RegionType searchRegion;
  typename TImage::RegionType iLPR = iConn->GetLargestPossibleRegion();
  typename TImage::RegionType jLPR = jConn->GetLargestPossibleRegion();
  for (IdentifierType d = 0; d < TImage::ImageDimension; d++)
  {
    searchRegion.SetIndex(d, iLPR.GetIndex()[d] - jLPR.GetIndex()[d] - jLPR.GetSize(d) + 1);
    searchRegion.SetSize(d, iLPR.GetSize(d) + jLPR.GetSize(d) - 2);
  }
  searchRegion.SetSize(axis, 1);
  searchRegion.SetIndex(axis, 0);
  typedef Image<bool, TImage::ImageDimension> BitmapType;
  BitmapType::Pointer                         searched = BitmapType::New();
  searched->SetRegions(searchRegion);
  searched->Allocate(true); // initialize to zero (false)

  // breadth first search starting from centroid
  std::queue<typename TImage::IndexType> uncomputed;
  uncomputed.push(ind);
  searched->SetPixel(ind, true);
  IdentifierType             score, maxScore = 0;
  typename TImage::IndexType bestIndex;

  while (!uncomputed.empty())
  {
    ind = uncomputed.front();
    uncomputed.pop();
    score = Intersection(iConn, iRegionId, jConn, jRegionIds, ind);
    if (score > maxScore)
    {
      maxScore = score;
      bestIndex = ind;
    }

    // we breadth this search
    if (!m_HeuristicAlignment || maxScore == 0 || score > maxScore / 2)
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
        }
        ind[d] += 2; //"right"
        if (searchRegion.IsInside(ind) && !searched->GetPixel(ind))
        {
          uncomputed.push(ind);
          searched->SetPixel(ind, true);
        }
        ind[d] -= 1; // return to initial
      }
    }
  }
  return bestIndex;
}

template <class TImage>
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

template <class TImage>
void
WriteDebug(typename TImage::Pointer out, const char * filename)
{
  typedef ImageFileWriter<TImage> WriterType;
  WriterType::Pointer             w = WriterType::New();
  w->SetInput(out);
  w->SetFileName(filename);
  try
  {
    w->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
  }
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::InterpolateBetweenTwo(int                             axis,
                                                                typename TImage *               out,
                                                                typename TImage::IndexValueType i,
                                                                typename TImage::IndexValueType j)
{
  if (i > j)
  {
    std::swap(i, j);
  }
  if (i == j || i + 1 == j)
  {
    return; // nothing to do
  }

  // compare slices i and j
  typename TImage::RegionType ri = m_TotalBoundingBox; // smaller than or equal to requested region
  ri.SetIndex(axis, i);
  ri.SetSize(axis, 1); // 1 slice
  typename TImage::RegionType rj = ri;
  rj.SetIndex(axis, j);

  BoolImageType::Pointer      eqResult = BoolImageType::New();
  typename TImage::RegionType rr = rj;
  rr.SetIndex(axis, 0);
  eqResult->CopyInformation(m_Input);
  eqResult->SetRegions(rr);
  eqResult->Allocate();

  typedef std::set<typename TImage::PixelType> LabelSetType;
  LabelSetType                                 overlaps; // which labels have overlaps from slice i to slice j

  ImageRegionConstIterator<TImage>   iti(m_Input, ri);
  ImageRegionConstIterator<TImage>   itj(m_Input, rj);
  ImageRegionIterator<BoolImageType> itr(eqResult, rr);
  while (!itr.IsAtEnd())
  {
    bool eq = (iti.Value() == itj.Value() && iti.Value() != 0); // are the pixels equal and non-zero?
    itr.Set(eq);
    if (eq) // exclude background label
    {
      overlaps.insert(iti.Value());
    }
    // next pixel
    ++iti;
    ++itj;
    ++itr;
  }
  // typedef unsigned char instead of bool to write it as nrrd
  // WriteDebug<BoolImageType>(eqResult,"C:\\Temp\\eqResult.nrrd");

  // for each label with overlaps determine inter-slice region correspondences
  for (LabelSetType::iterator it = overlaps.begin(); it != overlaps.end(); ++it)
  {
    if (m_Label != 0 && *it != m_Label)
      continue; // label was specified, and it was not this one, so skip

    // first determine disjoint regions
    ri = m_BoundingBoxes[*it]; // even smaller than m_TotalBoundingBox
    ri.SetSize(axis, 1);
    ri.SetIndex(axis, i);
    rj = ri;
    rj.SetIndex(axis, j);
    rr = rj;
    rr.SetIndex(axis, 0);

    // execute connected components
    IdentifierType           iCount, jCount;
    typename TImage::Pointer iconn = this->RegionedConnectedComponents(ri, *it, iCount);
    iconn->DisconnectPipeline();
    typename TImage::Pointer jconn = this->RegionedConnectedComponents(rj, *it, jCount);
    jconn->DisconnectPipeline();
    // WriteDebug<TImage>(iconn, "C:\\Temp\\iconn.nrrd");
    // WriteDebug<TImage>(jconn, "C:\\Temp\\jconn.nrrd");

    // go through comparison image and create correspondence pairs
    typedef std::set<std::pair<typename TImage::PixelType, typename TImage::PixelType>> PairSet;
    PairSet                                                                             pairs;
    itr.SetRegion(rr);
    itr.GoToBegin();
    ImageRegionConstIterator<TImage> iti(iconn, ri);
    ImageRegionConstIterator<TImage> itj(jconn, rj);
    while (!itr.IsAtEnd())
    {
      if (itr.Value() && iti.Value() != 0 && itj.Value() != 0)
      {
        pairs.insert(std::make_pair(iti.Value(), itj.Value()));
        // std::cout << "itr:" << itr.GetIndex() <<
        //   " iti:" << iti.GetIndex() << iti.Value() <<
        //   " itj:" << itj.GetIndex() << itj.Value() << std::endl;
      }
      ++iti;
      ++itj;
      ++itr; // next pixel
    }

    typedef std::map<typename TImage::PixelType, IdentifierType> CountMap;
    CountMap                                                     iCounts, jCounts;
    PairSet::iterator                                            p;
    for (p = pairs.begin(); p != pairs.end(); ++p)
    {
      iCounts[p->first]++;
      jCounts[p->second]++;
    }

    // first do extrapolation for components without overlaps
    CountMap::iterator iMapIt = iCounts.begin();
    for (IdentifierType ic = 1; ic <= iCount; ++ic) // component labels
    {
      if (iMapIt == iCounts.end() || ic < iMapIt->first)
      {
        Extrapolate(axis, out, *it, i, j, iconn, ic);
      }
      else // ic==iMapIt->first
      {
        ++iMapIt;
      }
    }
    CountMap::iterator jMapIt = jCounts.begin();
    for (IdentifierType jc = 1; jc <= jCount; ++jc) // component labels
    {
      if (jMapIt == jCounts.end() || jc < jMapIt->first)
      {
        Extrapolate(axis, out, *it, j, i, jconn, jc);
      }
      else // jc==jMapIt->first
      {
        ++jMapIt;
      }
    }

    // now handle 1 to 1 correspondences
    p = pairs.begin();
    while (p != pairs.end())
    {
      if (iCounts[p->first] == 1 && jCounts[p->second] == 1)
      {
        Interpolate1to1(axis, out, *it, i, j, iconn, p->first, jconn, p->second);
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
        for (PairSet::iterator rest = p; rest != pairs.end(); ++rest)
        {
          if (rest->second == p->second)
          {
            regionIDs.push_back(rest->first);
          }
        }

        Interpolate1toN(axis, out, *it, j, i, jconn, p->second, iconn, regionIDs);

        PairSet::iterator rest = p;
        ++rest;
        while (rest != pairs.end())
        {
          if (rest->second == p->second)
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
        pairs.erase(p++);
      } // M-to-1
      else if (jCounts[p->second] == 1) // 1-to-N
      {
        for (PairSet::iterator rest = p; rest != pairs.end(); ++rest)
        {
          if (rest->first == p->first)
          {
            regionIDs.push_back(rest->second);
          }
        }

        Interpolate1toN(axis, out, *it, i, j, iconn, p->first, jconn, regionIDs);

        PairSet::iterator rest = p;
        ++rest;
        while (rest != pairs.end())
        {
          if (rest->first == p->first)
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
      for (PairSet::iterator rest = p; rest != pairs.end(); ++rest)
      {
        if (rest->first == p->first)
        {
          regionIDs.push_back(rest->second);
        }
      }

      Interpolate1toN(axis, out, *it, i, j, iconn, p->first, jconn, regionIDs);

      PairSet::iterator rest = p;
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
  } // for each label with overlaps
} // void MorphologicalContourInterpolator::InterpolateBetweenTwo()

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::InterpolateAlong(int axis, typename TImage * out)
{
  SliceSetType aggregate;
  if (m_Label == 0) // all labels
  {
    for (typename LabeledSlicesType::iterator it = m_LabeledSlices[axis].begin(); it != m_LabeledSlices[axis].end();
         ++it)
    {
      aggregate.insert(it->second.begin(), it->second.end());
    }
  }
  else // we only care about m_Label
  {
    aggregate = m_LabeledSlices[axis][m_Label];
  }
  typename SliceSetType::iterator prev = aggregate.begin();
  if (prev == aggregate.end())
  {
    return; // nothing to do
  }

  typename SliceSetType::iterator it = aggregate.begin();
  for (++it; it != aggregate.end(); ++it)
  {
    InterpolateBetweenTwo(axis, out, *prev, *it);
    prev = it;
  }
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::GenerateData()
{
  m_Input = TImage::New();
  m_Input->Graft(const_cast<TImage *>(this->GetInput()));
  this->AllocateOutputs();
  m_Output = TImage::New();
  m_Output->Graft(this->GetOutput());
  // copy the input now, because interpolation is optimized using bounding boxes
  ImageAlgorithm::Copy(m_Input.GetPointer(),
                       m_Output.GetPointer(),
                       this->GetOutput()->GetRequestedRegion(),
                       this->GetOutput()->GetRequestedRegion());

  this->DetermineSliceOrientations();

  // merge all bounding boxes
  if (m_BoundingBoxes.size() == 0)
  {
    this->GraftOutput(m_Output);
    this->m_Output = ITK_NULLPTR;
    return; // nothing to process
  }
  else
  {
    m_TotalBoundingBox = MergeBoundingBoxes(m_BoundingBoxes);
  }

  if (m_Axis == -1) // interpolate along all axes
  {
    OrientationType aggregate = OrientationType();
    aggregate.Fill(false);

    if (this->m_Label == 0)
    {
      for (OrientationsType::iterator it = m_Orientations.begin(); it != m_Orientations.end(); ++it)
        for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
          aggregate[a] = aggregate[a] || it->second[a]; // any label needs interpolation along this axis
    }
    else
      aggregate = m_Orientations[m_Label]; // we only care about this label

    std::vector<TImage::Pointer> perAxisInterpolates;
    for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
    {
      if (aggregate[a])
      {
        TImage::Pointer imageA = TImage::New();
        imageA->CopyInformation(m_Output);
        imageA->SetRegions(m_Output->GetRequestedRegion());
        imageA->Allocate();
        this->InterpolateAlong(a, imageA);
        perAxisInterpolates.push_back(imageA);
      }
    }

    if (perAxisInterpolates.size() == 1)
    {
      m_Output = perAxisInterpolates[0];
      this->GraftOutput(m_Output);
      this->m_Output = ITK_NULLPTR;
      return;
    }
    // else
    std::vector<ImageRegionConstIterator<TImage>> iterators;

    for (int i = 0; i < perAxisInterpolates.size(); ++i)
    {
      ImageRegionConstIterator<TImage> it(perAxisInterpolates[i], m_Output->GetRequestedRegion());
      iterators.push_back(it);
    }

    std::vector<TImage::PixelType> values;
    values.reserve(perAxisInterpolates.size());

    ImageRegionIterator<TImage> it(m_Output, m_Output->GetRequestedRegion());
    while (!it.IsAtEnd())
    {
      values.clear();
      for (int i = 0; i < perAxisInterpolates.size(); ++i)
      {
        TImage::PixelType val = iterators[i].Value();
        if (val != 0)
          values.push_back(val);
      }

      if (values.size() == 0)
        it.Set(0); // all were zero
      else if (values.size() == 1)
        it.Set(values[0]); // the only non-zero
      else                 // median
      {
        std::nth_element(values.begin(), values.begin() + values.size() / 2, values.end());
        it.Set(values[values.size() / 2]);
      }

      // next pixel
      ++it;
      for (int i = 0; i < perAxisInterpolates.size(); ++i)
        ++(iterators[i]);
    }
  } // interpolate along all axes
  else // interpolate along the specified axis
  {
    this->InterpolateAlong(m_Axis, m_Output);
  }

  // put the output data back into the regular pipeline
  this->GraftOutput(m_Output);
  this->m_Output = ITK_NULLPTR;
}
} // namespace itk


#endif // itkMorphologicalContourInterpolator_hxx
