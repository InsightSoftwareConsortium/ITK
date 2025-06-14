/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMRASlabIdentifier_hxx
#define itkMRASlabIdentifier_hxx

#include <algorithm>
#include <vector>
#include <queue>
#include "itkImageRegionIterator.h"
#include "itkMath.h"

namespace itk
{
template <typename TInputImage>
MRASlabIdentifier<TInputImage>::MRASlabIdentifier()
  : m_Image(nullptr)
  , m_NumberOfSamples(10)
  , m_SlicingDirection(2)
  , m_BackgroundMinimumThreshold(NumericTraits<ImagePixelType>::min()) // default slicing axis is z

{}

template <typename TInputImage>
void
MRASlabIdentifier<TInputImage>::GenerateSlabRegions()
{
  // this method only works with 3D MRI image
  if (ImageType::ImageDimension != 3)
  {
    itkExceptionMacro("ERROR: This algorithm only works with 3D images.");
  }

  ImageSizeType   size;
  ImageRegionType region;
  ImageIndexType  index;

  region = m_Image->GetLargestPossibleRegion();
  size = region.GetSize();
  index = region.GetIndex();
  const IndexValueType firstSlice = index[m_SlicingDirection];
  const IndexValueType lastSlice = firstSlice + size[m_SlicingDirection];
  const SizeValueType  totalSlices = size[m_SlicingDirection];

  std::vector<double> avgMin(totalSlices);
  // calculate minimum intensities for each slice
  ImagePixelType pixel;
  for (int i = 0; i < 3; ++i)
  {
    if (i != m_SlicingDirection)
    {
      index[i] = 0;
    }
  }

  size[m_SlicingDirection] = 1;
  region.SetSize(size);

  SizeValueType  count = 0;
  IndexValueType currentSlice = firstSlice;
  while (currentSlice < lastSlice)
  {
    index[m_SlicingDirection] = currentSlice;
    region.SetIndex(index);

    ImageRegionConstIterator<TInputImage> iter(m_Image, region);
    iter.GoToBegin();

    std::priority_queue<ImagePixelType> mins;
    for (unsigned int i = 0; i < m_NumberOfSamples; ++i)
    {
      mins.push(NumericTraits<ImagePixelType>::max());
    }

    while (!iter.IsAtEnd())
    {
      pixel = iter.Get();
      if (pixel > m_BackgroundMinimumThreshold)
      {
        if (mins.top() > pixel)
        {
          mins.pop();
          mins.push(pixel);
        }
      }
      ++iter;
    }

    double sum = 0.0;
    while (!mins.empty())
    {
      sum += mins.top();
      mins.pop();
    }

    avgMin[count] = sum / static_cast<double>(m_NumberOfSamples);

    ++count;
    ++currentSlice;
  }

  // calculate overall average
  double sum = 0.0;
  auto   am_iter = avgMin.begin();
  while (am_iter != avgMin.end())
  {
    sum += *am_iter;
    ++am_iter;
  }

  const double average = sum / static_cast<double>(totalSlices);

  // determine slabs
  am_iter = avgMin.begin();

  double          prevSign = *am_iter - average;
  ImageRegionType slabRegion;

  SizeValueType  slabLength = 0;
  IndexValueType slabBegin = firstSlice;
  ImageSizeType  slabSize = size;
  ImageIndexType slabIndex = index;
  while (am_iter != avgMin.end())
  {
    double       avgMinValue = *am_iter;
    const double sign = avgMinValue - average;
    if ((sign * prevSign < 0) && (itk::Math::abs(sign) > m_Tolerance))
    {
      slabIndex[m_SlicingDirection] = slabBegin;
      slabSize[m_SlicingDirection] = slabLength;
      slabRegion.SetSize(slabSize);
      slabRegion.SetIndex(slabIndex);
      m_Slabs.push_back(slabRegion);

      prevSign = sign;
      slabBegin += slabLength;
      slabLength = 0;
    }
    ++am_iter;
    ++slabLength;
  }
  slabIndex[m_SlicingDirection] = slabBegin;
  slabSize[m_SlicingDirection] = slabLength;
  slabRegion.SetIndex(slabIndex);
  slabRegion.SetSize(slabSize);
  m_Slabs.push_back(slabRegion);
}

template <typename TInputImage>
auto
MRASlabIdentifier<TInputImage>::GetSlabRegionVector() -> SlabRegionVectorType
{
  return m_Slabs;
}

template <typename TInputImage>
void
MRASlabIdentifier<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (m_Image)
  {
    os << indent << "Image: " << m_Image << std::endl;
  }
  else
  {
    os << indent << "Image: "
       << "(None)" << std::endl;
  }
  os << indent << "NumberOfSamples: " << m_NumberOfSamples << std::endl;
  os << indent << "SlicingDirection: " << m_SlicingDirection << std::endl;
  os << indent << "Background Pixel Minimum Intensity Threshold: " << m_BackgroundMinimumThreshold << std::endl;
  os << indent << "Tolerance: " << m_Tolerance << std::endl;
}
} // end namespace itk

#endif /* itkMRASlabIdentifier_hxx */
