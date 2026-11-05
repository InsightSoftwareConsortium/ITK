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
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRandomConstIteratorWithOnlyIndex.h"
#include "itkImageRandomIteratorWithIndex.h"
#include "itkImageRandomNonRepeatingConstIteratorWithIndex.h"
#include "itkImageRandomNonRepeatingIteratorWithIndex.h"
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
#include "itkNeighborhoodIterator.h"
#include "itkShapedNeighborhoodIterator.h"

#include "itkImage.h"
#include "itkTestingGetBufferPointerCountingImage.h"
#include <gtest/gtest.h>
#include <type_traits> // For false_type and true_type.
#include <typeinfo>
#include <utility>

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


template <typename TIterator, typename = void>
struct Has_Set : std::false_type
{};

template <typename TIterator>
struct Has_Set<
  TIterator,
  std::void_t<decltype(std::declval<TIterator &>().Set(std::declval<const typename TIterator::PixelType &>()))>>
  : std::true_type
{};

template <typename TIterator, typename = void>
struct Has_MutableValue : std::false_type
{};

template <typename TIterator>
struct Has_MutableValue<TIterator, std::void_t<decltype(std::declval<TIterator &>().Value())>>
  : std::integral_constant<bool,
                           std::is_lvalue_reference_v<decltype(std::declval<TIterator &>().Value())> &&
                             !std::is_const_v<std::remove_reference_t<decltype(std::declval<TIterator &>().Value())>>>
{};

using TestImageType = itk::Image<int, 2>;
using ConstQualifiedRegionIterator = itk::ImageRegionIterator<const TestImageType>;
using MutableRegionIterator = itk::ImageRegionIterator<TestImageType>;

static_assert(
  std::is_constructible_v<ConstQualifiedRegionIterator, const TestImageType *, const TestImageType::RegionType &>);
static_assert(std::is_same_v<decltype(std::declval<const ConstQualifiedRegionIterator &>().Get()), int>);
static_assert(std::is_same_v<decltype(std::declval<ConstQualifiedRegionIterator &>().Value()), const int &>);
static_assert(!Has_Set<ConstQualifiedRegionIterator>::value);
static_assert(!Has_MutableValue<ConstQualifiedRegionIterator>::value);
static_assert(Has_Set<MutableRegionIterator>::value);
static_assert(Has_MutableValue<MutableRegionIterator>::value);

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


template <template <typename> typename TIteratorTemplate, typename TImage>
void
CheckIteratorSupportsClassTemplateArgumentDeduction()
{
  using RegionType = typename TImage::RegionType;
  using PointerType = typename TImage::Pointer;
  static_assert(std::is_same_v<decltype(TIteratorTemplate(PointerType{}, RegionType{})),
                               decltype(TIteratorTemplate<TImage>(PointerType{}, RegionType{}))>);
}


template <template <typename> typename TIteratorTemplate, typename TImage>
void
CheckConstIteratorSupportsClassTemplateArgumentDeduction()
{
  using RegionType = typename TImage::RegionType;
  using ConstPointerType = typename TImage::ConstPointer;
  static_assert(std::is_same_v<decltype(TIteratorTemplate(ConstPointerType{}, RegionType{})),
                               decltype(TIteratorTemplate<TImage>(ConstPointerType{}, RegionType{}))>);
}


template <template <typename> typename... TIteratorTemplate>
void
CheckIteratorsSupportClassTemplateArgumentDeduction()
{
  (CheckIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<int>>(), ...);
  (CheckIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<double, 3>>(), ...);
}


template <template <typename> typename... TIteratorTemplate>
void
CheckConstIteratorsSupportClassTemplateArgumentDeduction()
{
  (CheckConstIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<int>>(), ...);
  (CheckConstIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<double, 3>>(), ...);
}


template <typename TIterator>
void
ExpectConstructorCallsNonConstGetBufferPointer()
{
  EXPECT_TRUE(itk::Testing::IteratorConstructorCallsNonConstGetBufferPointer<TIterator>()) << typeid(TIterator).name();
}


template <template <typename> typename... TIteratorTemplate>
void
CheckIteratorsCallNonConstGetBufferPointer()
{
  (ExpectConstructorCallsNonConstGetBufferPointer<
     TIteratorTemplate<itk::Testing::GetBufferPointerCountingImage<int>>>(),
   ...);
}
} // namespace


// Checks that an iterator that is just constructed by `IteratorType(image, region)` is at the begin.
TEST(ImageIterators, ConstructedAtBegin)
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


// Checks that the iterator class templates support class template argument deduction (CTAD), when constructed by
// `IteratorTemplate(ptr, region)`, with `ptr` being a `SmartPointer` to an image.
TEST(ImageIterators, SupportClassTemplateArgumentDeduction)
{
  CheckIteratorsSupportClassTemplateArgumentDeduction<itk::ImageConstIterator,
                                                      itk::ImageConstIteratorWithIndex,
                                                      itk::ImageConstIteratorWithOnlyIndex,
                                                      itk::ImageIterator,
                                                      itk::ImageIteratorWithIndex,
                                                      itk::ImageLinearConstIteratorWithIndex,
                                                      itk::ImageLinearIteratorWithIndex,
                                                      itk::ImageRandomConstIteratorWithIndex,
                                                      itk::ImageRandomConstIteratorWithOnlyIndex,
                                                      itk::ImageRandomIteratorWithIndex,
                                                      itk::ImageRandomNonRepeatingConstIteratorWithIndex,
                                                      itk::ImageRandomNonRepeatingIteratorWithIndex,
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

  CheckConstIteratorsSupportClassTemplateArgumentDeduction<itk::ImageConstIterator,
                                                           itk::ImageConstIteratorWithIndex,
                                                           itk::ImageConstIteratorWithOnlyIndex,
                                                           itk::ImageLinearConstIteratorWithIndex,
                                                           itk::ImageRandomConstIteratorWithIndex,
                                                           itk::ImageRandomConstIteratorWithOnlyIndex,
                                                           itk::ImageRandomNonRepeatingConstIteratorWithIndex,
                                                           itk::ImageRegionConstIterator,
                                                           itk::ImageRegionConstIteratorWithIndex,
                                                           itk::ImageRegionConstIteratorWithOnlyIndex,
                                                           itk::ImageRegionExclusionConstIteratorWithIndex,
                                                           itk::ImageRegionReverseConstIterator,
                                                           itk::ImageReverseConstIterator,
                                                           itk::ImageScanlineConstIterator,
                                                           itk::ImageSliceConstIteratorWithIndex>();
}


// Checks that a const-qualified image region iterator only provides read access.
TEST(ImageIterators, ConstQualifiedRegionIteratorIsReadOnly)
{
  const auto image = TestImageType::New();
  image->SetRegions(TestImageType::SizeType::Filled(1));
  image->Allocate();
  image->FillBuffer(1);

  const TestImageType::ConstPointer  constImage = image;
  const ConstQualifiedRegionIterator iterator(constImage, constImage->GetBufferedRegion());
  EXPECT_EQ(iterator.Get(), 1);
  EXPECT_EQ(iterator.Value(), 1);
}

// Checks that non-const iterators call the non-const GetBufferPointer() overload of the image on construction.
TEST(ImageIterators, ConstructorCallsNonConstGetBufferPointer)
{
  CheckIteratorsCallNonConstGetBufferPointer<itk::ImageIterator,
                                             itk::ImageIteratorWithIndex,
                                             itk::ImageLinearIteratorWithIndex,
                                             itk::ImageRandomIteratorWithIndex,
                                             itk::ImageRandomNonRepeatingIteratorWithIndex,
                                             itk::ImageRegionExclusionIteratorWithIndex,
                                             itk::ImageRegionIterator,
                                             itk::ImageRegionIteratorWithIndex,
                                             itk::ImageRegionReverseIterator,
                                             itk::ImageReverseIterator,
                                             itk::ImageScanlineIterator,
                                             itk::ImageSliceIteratorWithIndex>();

  using ImageType = itk::Testing::GetBufferPointerCountingImage<int>;
  const auto radius = ImageType::SizeType::Filled(1);
  EXPECT_TRUE(
    itk::Testing::IteratorConstructorCallsNonConstGetBufferPointer<itk::NeighborhoodIterator<ImageType>>(radius));
  EXPECT_TRUE(
    itk::Testing::IteratorConstructorCallsNonConstGetBufferPointer<itk::ShapedNeighborhoodIterator<ImageType>>(radius));
}


// Checks that ImageRegionReverseIterator(const ImageConstIterator &) calls the non-const GetBufferPointer() overload.
TEST(ImageIterators, ReverseIteratorConversionCallsNonConstGetBufferPointer)
{
  using ImageType = itk::Testing::GetBufferPointerCountingImage<int>;
  const auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(4));
  image->Allocate();
  const itk::ImageRegionConstIterator<ImageType> constIterator(image, image->GetBufferedRegion());

  const size_t numberOfCallsBefore = image->GetNumberOfNonConstGetBufferPointerCalls();
  [[maybe_unused]] const itk::ImageRegionReverseIterator<ImageType> reverseIterator(constIterator);
  EXPECT_GT(image->GetNumberOfNonConstGetBufferPointerCalls(), numberOfCallsBefore);
}
