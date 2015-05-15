#ifndef __itkMorphologicalContourInterpolator_hxx
#define __itkMorphologicalContourInterpolator_hxx

#include "itkMorphologicalContourInterpolator.h"
#include "itkObjectFactory.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include <utility>
#include <algorithm>

namespace itk
{
template <class TImage>
void
MorphologicalContourInterpolator<TImage>::ExpandRegion(typename TImage::RegionType & region,
                                                       typename TImage::IndexType    index)
{
  for (int a = 0; a < TImage::ImageDimension; a++)
  {
    if (region.GetIndex(a) > index[a])
      region.SetIndex(a, index[a]);
    else if (region.GetIndex(a) + region.GetSize(a) <= index[a])
      region.SetSize(a, index[a] - region.GetIndex(a) + 1);
    // else it is already within
  }
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::DetermineSliceOrientations()
{
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::Pointer      output = this->GetOutput();
  m_Orientations.clear();

  typename TImage::RegionType region = output->GetRequestedRegion();
  typename TImage::RegionType lpr = input->GetLargestPossibleRegion();
  for (int a = 0; a < TImage::ImageDimension; a++)
  {
    if (region.GetIndex(a) < lpr.GetIndex(a) + 1)
      region.SetIndex(a, lpr.GetIndex(a) + 1);
    if (region.GetIndex(a) + region.GetSize(a) > lpr.GetIndex(a) + lpr.GetSize(a) - 1)
      region.SetSize(a, lpr.GetIndex(a) + lpr.GetSize(a) - 1 - region.GetIndex(a));
  }
  ImageRegionConstIteratorWithIndex<TImage> it(input, region);

  OrientationType ors = OrientationType();

  while (!it.IsAtEnd())
  {
    typename TImage::IndexType indPrev, indNext, ind = it.GetIndex();
    typename TImage::PixelType val = input->GetPixel(ind);
    if (val != 0 || (m_Label != 0 && val == m_Label))
    {
      typename TImage::RegionType bb1;
      bb1.SetIndex(ind);
      for (int a = 0; a < TImage::ImageDimension; a++)
        bb1.SetSize(a, 1);
      std::pair<BoundingBoxesType::iterator, bool> resBB = m_BoundingBoxes.insert(std::make_pair(val, bb1));
      if (!resBB.second) // include this index in existing BB
        ExpandRegion(resBB.first->second, ind);

      std::pair<OrientationsType::iterator, bool> res = m_Orientations.insert(std::make_pair(val, ors));
      OrientationsType::iterator                  oRef = res.first;
      int                                         cTrue = 0;
      for (int a = 0; a < TImage::ImageDimension; a++)
      {
        indPrev = ind;
        indPrev[a]--;
        indNext = ind;
        indNext[a]++;
        {
          if (input->GetPixel(indPrev) == 0 && input->GetPixel(indNext) == 0)
          {
            ors[a] = true;
            cTrue++;
          }
        }
      }
      if (cTrue == 1) // slice has empty adjacent space only along one axis
      {
        for (int a = 0; a < TImage::ImageDimension; a++)
          oRef->second[a] = oRef->second[a] || ors[a]; // add this dimension for this label
      }
    }
    ++it;
  }

  // widen bounding boxes by 1
  region = output->GetRequestedRegion();
  for (BoundingBoxesType::iterator it = m_BoundingBoxes.begin(); it != m_BoundingBoxes.end(); ++it)
  {
    for (int a = 0; a < TImage::ImageDimension; a++)
    {
      if (it->second.GetIndex(a) - 1 <= region.GetIndex(a))
        it->second.SetIndex(a, it->second.GetIndex(a) - 1);
      if (it->second.GetIndex(a) + it->second.GetSize(a) + 1 <= region.GetIndex(a) + region.GetSize(a))
        it->second.SetSize(a, it->second.GetSize(a) + 1);
    }
  }
}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::InterpolateAlong(int axis, typename TImage::Pointer out)
{}

template <class TImage>
void
MorphologicalContourInterpolator<TImage>::GenerateData()
{
  typename TImage::ConstPointer input = this->GetInput();
  typename TImage::Pointer      output = this->GetOutput();
  this->AllocateOutputs();

  if (m_Axis == -1)
  {
    this->DetermineSliceOrientations();

    OrientationType aggregate = OrientationType();
    if (this->m_Label == 0)
    {
      for (OrientationsType::iterator it = m_Orientations.begin(); it != m_Orientations.end(); ++it)
        for (int a = 0; a < TImage::ImageDimension; a++)
          aggregate[a] = aggregate[a] || it->second[a]; // any label needs interpolation along this axis
    }
    else
      aggregate = m_Orientations[m_Label]; // we only care about this label

    std::vector<TImage::Pointer> perAxisInterpolates;
    for (int a = 0; a < TImage::ImageDimension; a++)
      if (aggregate[a])
      {
        TImage::Pointer imageA = TImage::New();
        imageA->CopyInformation(output);
        imageA->Allocate();
        InterpolateAlong(a, imageA);
        perAxisInterpolates.push_back(imageA);
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
      else                 // median, gives preference to higher-numbered axis
      {
        std::nth_element(values.begin(), values.begin() + values.size() / 2, values.end());
        it.Set(values[values.size() / 2]);
      }

      // next pixel
      ++it;
      for (int i = 0; i < perAxisInterpolates.size(); i++)
        ++(iterators[i]);
    }
  }
  else
    InterpolateAlong(m_Axis, output);
}
} // namespace itk


#endif //__itkMorphologicalContourInterpolator_hxx
