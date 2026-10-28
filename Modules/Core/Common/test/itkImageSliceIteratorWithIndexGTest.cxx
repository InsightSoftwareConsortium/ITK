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

#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageSliceIteratorWithIndex.h"
#include "itkImage.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include <gtest/gtest.h>
#include <cstdint>
#include <vector>

namespace
{
template <typename TIterator>
class InspectableSliceIterator : public TIterator
{
public:
  using TIterator::TIterator;

  const typename TIterator::InternalPixelType *
  Position() const
  {
    return this->m_Position;
  }
};

template <typename TIterator, typename TImage>
void
CheckTraversal(TImage * image, const typename TImage::RegionType & region, unsigned int first, unsigned int second)
{
  InspectableSliceIterator<TIterator> it(image, region);
  it.SetFirstDirection(first);
  it.SetSecondDirection(second);
  const auto capacity = image->GetPixelContainer()->Capacity();
  const auto buffer = image->GetBufferPointer();
  const auto begin = reinterpret_cast<std::uintptr_t>(buffer);
  const auto end = reinterpret_cast<std::uintptr_t>(buffer + capacity);
  const auto checkPosition = [&] {
    const auto address = reinterpret_cast<std::uintptr_t>(it.Position());
    ASSERT_GE(address, begin);
    ASSERT_LE(address, end);
  };

  for (const bool reverse : { false, true })
  {
    SCOPED_TRACE(reverse);
    std::vector<unsigned int> visits(capacity, 0);
    if (reverse)
    {
      it.GoToReverseBegin();
    }
    else
    {
      it.GoToBegin();
    }
    while (!(reverse ? it.IsAtReverseEnd() : it.IsAtEnd()))
    {
      while (!(reverse ? it.IsAtReverseEndOfSlice() : it.IsAtEndOfSlice()))
      {
        while (!(reverse ? it.IsAtReverseEndOfLine() : it.IsAtEndOfLine()))
        {
          ASSERT_TRUE(region.IsInside(it.GetIndex()));
          const auto offset = image->ComputeOffset(it.GetIndex());
          ASSERT_EQ(it.Position(), buffer + offset);
          ASSERT_EQ(it.Get(), image->GetPixel(it.GetIndex()));
          ASSERT_EQ(visits[offset]++, 0u);
          if (reverse)
          {
            --it;
          }
          else
          {
            ++it;
          }
          ASSERT_NO_FATAL_FAILURE(checkPosition());
        }
        if (reverse)
        {
          ++it;
          ASSERT_EQ(it.Position(), buffer + image->ComputeOffset(it.GetIndex()));
          --it;
        }
        else
        {
          --it;
          ASSERT_EQ(it.Position(), buffer + image->ComputeOffset(it.GetIndex()));
          ++it;
        }
        ASSERT_NO_FATAL_FAILURE(checkPosition());
        if (reverse)
        {
          it.PreviousLine();
        }
        else
        {
          it.NextLine();
        }
        ASSERT_NO_FATAL_FAILURE(checkPosition());
      }
      if (reverse)
      {
        it.PreviousSlice();
      }
      else
      {
        it.NextSlice();
      }
      ASSERT_NO_FATAL_FAILURE(checkPosition());
    }
    itk::ImageRegionConstIteratorWithIndex<TImage> expected(image, image->GetBufferedRegion());
    for (expected.GoToBegin(); !expected.IsAtEnd(); ++expected)
    {
      ASSERT_EQ(visits[image->ComputeOffset(expected.GetIndex())], region.IsInside(expected.GetIndex()) ? 1u : 0u);
    }
  }
  it.GoToBegin();
  while (!it.IsAtEndOfLine())
  {
    ++it;
  }
  it.NextLine();
  it.GoToBeginOfSlice();
  ASSERT_EQ(it.GetIndex(), region.GetIndex());
  ASSERT_EQ(it.Position(), buffer + image->ComputeOffset(it.GetIndex()));
}

template <unsigned int VDimension>
void
CheckDirections()
{
  using ImageType = itk::Image<unsigned int, VDimension>;
  auto                          image = ImageType::New();
  typename ImageType::IndexType begin;
  typename ImageType::SizeType  size;
  for (unsigned int d = 0; d < VDimension; ++d)
  {
    begin[d] = static_cast<itk::IndexValueType>(d) - 2;
    size[d] = d + 2;
  }
  image->SetRegions(typename ImageType::RegionType(begin, size));
  image->Allocate();
  for (itk::SizeValueType i = 0; i < image->GetBufferedRegion().GetNumberOfPixels(); ++i)
  {
    image->GetBufferPointer()[i] = static_cast<unsigned int>(i);
  }
  for (unsigned int first = 0; first < VDimension; ++first)
  {
    for (unsigned int second = 0; second < VDimension; ++second)
    {
      if (first == second)
      {
        continue;
      }
      SCOPED_TRACE(first);
      SCOPED_TRACE(second);
      for (unsigned int shape = 0; shape < 4; ++shape)
      {
        SCOPED_TRACE(shape);
        auto region = image->GetBufferedRegion();
        auto subregionSize = region.GetSize();
        auto subregionBegin = region.GetIndex();
        for (unsigned int d = 0; d < VDimension; ++d)
        {
          if (shape == 3 || (shape == 1 && d == first) || (shape == 2 && d == second))
          {
            subregionSize[d] = 1;
            ++subregionBegin[d];
          }
        }
        region.SetSize(subregionSize);
        region.SetIndex(subregionBegin);
        ASSERT_NO_FATAL_FAILURE((
          CheckTraversal<itk::ImageSliceConstIteratorWithIndex<ImageType>>(image.GetPointer(), region, first, second)));
        ASSERT_NO_FATAL_FAILURE(
          (CheckTraversal<itk::ImageSliceIteratorWithIndex<ImageType>>(image.GetPointer(), region, first, second)));
      }
    }
  }
}
} // namespace

TEST(ImageSliceIteratorWithIndex, BoundedPointersForEveryDirection2D) { ASSERT_NO_FATAL_FAILURE(CheckDirections<2>()); }

TEST(ImageSliceIteratorWithIndex, BoundedPointersForEveryDirection3D) { ASSERT_NO_FATAL_FAILURE(CheckDirections<3>()); }

TEST(ImageSliceIteratorWithIndex, BoundedPointersForEveryDirection4D) { ASSERT_NO_FATAL_FAILURE(CheckDirections<4>()); }
