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

// First include the header files to be tested:
#include "itkImageConstIterator.h"
#include "itkImageConstIteratorWithIndex.h"
#include "itkImageConstIteratorWithOnlyIndex.h"
#include "itkImageIterator.h"
#include "itkImageIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithOnlyIndex.h"
#include "itkImageRegionExclusionConstIteratorWithIndex.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionReverseConstIterator.h"
#include "itkImageRegionReverseIterator.h"
#include "itkImageReverseConstIterator.h"
#include "itkImageReverseIterator.h"
#include "itkImageScanlineConstIterator.h"
#include "itkImageScanlineIterator.h"
#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageSliceIteratorWithIndex.h"

#include "itkImage.h"
#include <gtest/gtest.h>
#include <type_traits> // For false_type and true_type.

namespace
{
// Has_IsAtBegin tells whether the specified iterator type has an `IsAtBegin()` member function, returning a `bool`. It
// is inspired by "How to detect whether there is a specific member variable in class?", answered by Andy Prowl, Jan 25,
// 2013 at https://stackoverflow.com/a/14523787 (the `has_id` example).
template <typename TIterator, typename = bool>
struct Has_IsAtBegin : std::false_type
{};

template <typename TIterator>
struct Has_IsAtBegin<TIterator, decltype(std::declval<TIterator>().IsAtBegin())> : std::true_type
{};


template <typename TIterator>
void
CheckConstructedAtBegin()
{
  using ImageType = typename TIterator::ImageType;
  using IndexType = typename TIterator::IndexType;
  using SizeType = typename TIterator::SizeType;
  using RegionType = typename TIterator::RegionType;

  const auto image = ImageType::New();

  // Use a small image size, so that the unit test won't take a long time.
  static constexpr itk::SizeValueType imageSizeValue{ 4 };

  image->SetRegions(SizeType::Filled(imageSizeValue));
  image->Allocate();

  // Check various regions, specified by the following `indexValue` and `sizeValue` combinations:
  for (const itk::IndexValueType indexValue : { 0, 1 })
  {
    for (const auto sizeValue : { itk::SizeValueType{ 1 }, imageSizeValue - 1 })
    {
      const RegionType imageRegion(IndexType::Filled(indexValue), SizeType::Filled(sizeValue));

      const TIterator iterator(image, imageRegion);
      TIterator       iteratorThatGoesToBegin = iterator;
      iteratorThatGoesToBegin.GoToBegin();
      EXPECT_EQ(iterator, iteratorThatGoesToBegin);

      if constexpr (Has_IsAtBegin<TIterator>::value)
      {
        // Extra check, using IsAtBegin(), if the iterator has that member function. (Some iterators
        // do not have an IsAtBegin() member function, for example, ImageRegionConstIteratorWithIndex.)
        EXPECT_TRUE(TIterator(image, imageRegion).IsAtBegin());
      }
    }
  }
}


template <template <typename> typename... TIteratorTemplate>
void
CheckIteratorsConstructedAtBegin()
{
  (CheckConstructedAtBegin<TIteratorTemplate<itk::Image<int>>>(), ...);
  (CheckConstructedAtBegin<TIteratorTemplate<itk::Image<double, 3>>>(), ...);
}
} // namespace


// Checks that an iterator that is just constructed by `IteratorType(image, region)` is at the begin.
TEST(ImageRegionIterator, IsConstructedAtBegin)
{
  CheckIteratorsConstructedAtBegin<itk::ImageConstIterator,
                                   itk::ImageConstIteratorWithIndex,
                                   itk::ImageConstIteratorWithOnlyIndex,
                                   itk::ImageIterator,
                                   itk::ImageIteratorWithIndex,
                                   itk::ImageLinearConstIteratorWithIndex,
                                   itk::ImageLinearIteratorWithIndex,
                                   itk::ImageRegionConstIterator,
                                   itk::ImageRegionConstIteratorWithIndex,
                                   itk::ImageRegionConstIteratorWithOnlyIndex,
                                   itk::ImageRegionExclusionConstIteratorWithIndex,
                                   itk::ImageRegionExclusionIteratorWithIndex,
                                   itk::ImageRegionIterator,
                                   itk::ImageRegionIteratorWithIndex,
                                   itk::ImageRegionReverseConstIterator,
                                   itk::ImageRegionReverseIterator,
                                   itk::ImageReverseConstIterator,
                                   itk::ImageReverseIterator,
                                   itk::ImageScanlineConstIterator,
                                   itk::ImageScanlineIterator,
                                   itk::ImageSliceConstIteratorWithIndex,
                                   itk::ImageSliceIteratorWithIndex>();
}
