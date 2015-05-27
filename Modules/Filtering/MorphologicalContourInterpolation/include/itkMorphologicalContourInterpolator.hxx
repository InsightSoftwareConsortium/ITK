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
#include "itkConnectedComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include <utility>
#include <algorithm>

namespace itk
{
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
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::SetLabeledSliceIndices(unsigned int                                 axis,
                                                                 std::vector<typename TImage::IndexValueType> indices)
{
  m_LabeledSlices[axis] = SliceSetType().insert(indices.begin(), indices.end());
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
  typename const TImage * input = this->GetInput();
  typename TImage *       output = this->GetOutput();
  m_LabeledSlices.clear();
  m_LabeledSlices.resize(TImage::ImageDimension); // initialize with empty sets
  m_BoundingBoxes.clear();
  m_Orientations.clear();

  typename TImage::RegionType region = output->GetRequestedRegion();
  typename TImage::RegionType largestPossibleRegion = input->GetLargestPossibleRegion();
  for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
  {
    if (region.GetIndex(a) < largestPossibleRegion.GetIndex(a) + 1)
    {
      region.SetIndex(a, largestPossibleRegion.GetIndex(a) + 1);
    }
    if (region.GetIndex(a) + region.GetSize(a) >
        largestPossibleRegion.GetIndex(a) + largestPossibleRegion.GetSize(a) - 1)
    {
      region.SetSize(a, largestPossibleRegion.GetIndex(a) + largestPossibleRegion.GetSize(a) - 1 - region.GetIndex(a));
    }
  }
  ImageRegionConstIteratorWithIndex<TImage> it(input, region);

  OrientationType orientations = OrientationType();
  orientations.Fill(false);

  while (!it.IsAtEnd())
  {
    typename TImage::IndexType       indPrev, indNext;
    typename const TImage::IndexType ind = it.GetIndex();
    typename const TImage::PixelType val = input->GetPixel(ind);
    if (val != 0 || (m_Label != 0 && val == m_Label))
    {
      typename TImage::RegionType boundingBox1;
      boundingBox1.SetIndex(ind);
      for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
        boundingBox1.SetSize(a, 1);
      std::pair<BoundingBoxesType::iterator, bool> resBB = m_BoundingBoxes.insert(std::make_pair(val, boundingBox1));
      if (!resBB.second) // include this index in existing BB
        ExpandRegion(resBB.first->second, ind);

      std::pair<OrientationsType::iterator, bool> res = m_Orientations.insert(std::make_pair(val, orientations));
      OrientationsType::iterator                  oRef = res.first;
      unsigned int                                cTrue = 0, cAdjacent = 0, axis;
      for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
      {
        indPrev = ind;
        indPrev[a]--;
        indNext = ind;
        indNext[a]++;
        const typename TImage::PixelType prev = input->GetPixel(indPrev);
        const typename TImage::PixelType next = input->GetPixel(indNext);
        if (prev == 0 && next == 0)
        {
          axis = a;
          cTrue++;
        }
        else if (prev == val && next == val)
        {
          cAdjacent++;
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
    ++it;
  }

  // widen bounding boxes by 1
  region = output->GetRequestedRegion();
  for (BoundingBoxesType::iterator it = m_BoundingBoxes.begin(); it != m_BoundingBoxes.end(); ++it)
  {
    for (unsigned int a = 0; a < TImage::ImageDimension; ++a)
    {
      if (it->second.GetIndex(a) - 1 <= region.GetIndex(a))
        it->second.SetIndex(a, it->second.GetIndex(a) - 1);
      if (it->second.GetIndex(a) + it->second.GetSize(a) + 1 <= region.GetIndex(a) + region.GetSize(a))
        it->second.SetSize(a, it->second.GetSize(a) + 1);
    }
  }
}

template <class TImage>
typename TImage::Pointer
MorphologicalContourInterpolator<TImage>::RegionedConnectedComponents(typename TImage::RegionType region,
                                                                      typename TImage::PixelType  label,
                                                                      itk::IdentifierType &       objectCount)
{
  typedef BinaryThresholdImageFilter<typename TImage, BoolImageType> BinarizerType;
  BinarizerType::Pointer                                             bin = BinarizerType::New();
  bin->SetLowerThreshold(label);
  bin->SetUpperThreshold(label);
  bin->SetInput(this->GetInput());
  bin->GetOutput()->SetRequestedRegion(region);
  typedef ConnectedComponentImageFilter<BoolImageType, typename TImage> ConnComponentsType;
  ConnComponentsType::Pointer                                           conn = ConnComponentsType::New();
  conn->SetInput(bin->GetOutput());
  conn->SetFullyConnected(true);
  conn->Update();
  return conn->GetOutput();
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::InterpolateBetweenTwo(int                             axis,
                                                                typename TImage *               out,
                                                                typename TImage::IndexValueType i,
                                                                typename TImage::IndexValueType j)
{
  if (i > j)
    std::swap(i, j);
  if (i == j || i + 1 == j)
    return; // nothing to do

  // compare slices i and j
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::RegionType   ri = m_TotalBoundingBox; // smaller than or equal to requested region
  ri.SetIndex(axis, i);
  ri.SetSize(axis, 1); // 1 slice
  typename TImage::RegionType rj = ri;
  rj.SetIndex(axis, j);

  BoolImageType::Pointer      eqResult = BoolImageType::New();
  typename TImage::RegionType rr = rj;
  rr.SetIndex(axis, 0);
  eqResult->CopyInformation(input);
  eqResult->SetRegions(rr);
  eqResult->Allocate();

  typedef std::set<typename TImage::PixelType> LabelSetType;
  LabelSetType                                 overlaps; // which labels have overlaps from slice i to slice j

  ImageRegionConstIterator<TImage>   iti(input, ri);
  ImageRegionConstIterator<TImage>   itj(input, rj);
  ImageRegionIterator<BoolImageType> itr(eqResult, rr);
  while (!itr.IsAtEnd())
  {
    bool eq = (iti.Value() == itj.Value()); // are the pixels equal?
    itr.Set(eq);
    if (eq)
      overlaps.insert(iti.Value());
    ++iti;
    ++itj;
    ++itr; // next pixel
  }

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
    typename TImage::Pointer jconn = this->RegionedConnectedComponents(rj, *it, jCount);

    // go through comparison image and create correspondence pairs
    typedef std::set<std::pair<typename TImage::PixelType, typename TImage::PixelType>> PairSet;
    PairSet                                                                             pairs;
    itr.GoToBegin();
    ImageRegionConstIterator<TImage> iti(iconn, ri);
    ImageRegionConstIterator<TImage> itj(jconn, rj);
    while (!itr.IsAtEnd())
    {
      if (itr.Value())
      {
        pairs.insert(std::make_pair(iti.Value(), itj.Value()));
      }
      ++iti;
      ++itj;
      ++itr; // next pixel
    }

    typedef std::map<typename TImage::PixelType, IdentifierType> CountMap;
    CountMap                                                     iCounts, jCounts;
    for (PairSet::iterator p = pairs.begin(); p != pairs.end(); ++p)
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
        extrapolate(); // TODO: pass correct parameters and implement
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
        extrapolate(); // TODO: pass correct parameters and implement
      }
      else // jc==jMapIt->first
      {
        ++jMapIt;
      }
    }

    // now handle 1 to 1 correspondences
    PairSet::iterator p = pairs.begin();
    while (p != pairs.end())
    {
      if (iCounts[p->first] == 1 && jCounts[p->second] == 1)
      {
        interpolate1to1(); // TODO: pass correct parameters and implement
        iCounts.erase(p->first);
        jCounts.erase(p->second);
        pairs.erase(p++);
      }
      else
      {
        ++p;
      }
    }

    // now do 1-to-N and M-to-1 cases
    p = pairs.begin();
    while (p != pairs.end())
    {
      if (iCounts[p->first] == 1) // 1-to-N
      {
        interpolate1toN(); // TODO: pass correct parameters and implement
        PairSet::iterator rest = p;
        ++rest;
        while (rest != pairs.end())
        {
          if (rest->second == p->second)
          {
            pairs.erase(rest++);
          }
          else
          {
            ++rest;
          }
        }
        iCounts.erase(p->first);
        jCounts.erase(p->second);
        pairs.erase(p++);
      }
      else if (jCounts[p->second] == 1) // M-to-1
      {
        interpolate1toN(); // TODO: pass correct parameters and implement
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
        iCounts.erase(p->first);
        jCounts.erase(p->second);
        pairs.erase(p++);
      }
      else
      {
        ++p;
      }
    }

    // only M-to-N correspondences remain
    // we turn each M-to-N case into m 1-to-N cases
    p = pairs.begin();
    while (p != pairs.end())
    {
      // TODO: enumerate the labels which correspond to p->first and make it 1-to-N
      interpolate1toN(); // TODO: pass correct parameters and implement
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
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::Pointer      output = this->GetOutput();
  this->AllocateOutputs();
  output->FillBuffer(0); // clear the image now, because interpolation is optimized using bounding boxes

  this->DetermineSliceOrientations();

  // merge all bounding boxes
  if (m_BoundingBoxes.size() == 0)
  {
    return; // nothing to process
  }
  else
  {
    m_TotalBoundingBox = m_BoundingBoxes.begin()->second;
  }
  for (BoundingBoxesType::iterator it = m_BoundingBoxes.begin(); it != m_BoundingBoxes.end(); ++it)
  {
    ExpandRegion(m_TotalBoundingBox, it->second.GetIndex());
    ExpandRegion(m_TotalBoundingBox, it->second.GetIndex() + it->second.GetSize());
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
        imageA->CopyInformation(output);
        imageA->SetRegions(output->GetRequestedRegion());
        imageA->Allocate();
        this->InterpolateAlong(a, imageA);
        perAxisInterpolates.push_back(imageA);
      }
    }

    if (perAxisInterpolates.size() == 1)
    {
      output = perAxisInterpolates[0];
      return;
    }
    // else
    std::vector<ImageRegionConstIterator<TImage>> iterators;

    for (int i = 0; i < perAxisInterpolates.size(); i++)
    {
      ImageRegionConstIterator<TImage> it(perAxisInterpolates[i], output->GetRequestedRegion());
      iterators.push_back(it);
    }

    std::vector<TImage::PixelType> values;
    values.reserve(perAxisInterpolates.size());

    ImageRegionIterator<TImage> it(output, output->GetRequestedRegion());
    while (!it.IsAtEnd())
    {
      values.clear();
      for (int i = 0; i < perAxisInterpolates.size(); i++)
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
      for (int i = 0; i < perAxisInterpolates.size(); i++)
        ++(iterators[i]);
    }
  } // interpolate along all axes
  else // interpolate along the specified axis
  {
    this->InterpolateAlong(m_Axis, output);
  }
}
} // namespace itk


#endif // itkMorphologicalContourInterpolator_hxx
