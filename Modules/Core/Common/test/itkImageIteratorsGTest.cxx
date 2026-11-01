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
#include "itkLineIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkShapedNeighborhoodIterator.h"

#include "itkImage.h"
#include "itkTestingGetBufferPointerCountingImage.h"
#include <gtest/gtest.h>
#include <memory>
#include <type_traits> // For false_type and true_type.
#include <typeinfo>

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

template <typename T, typename U, typename = void>
struct is_comparable : std::false_type
{};

template <typename T, typename U>
struct is_comparable<T,
                     U,
                     std::void_t<decltype(std::declval<const T &>() == std::declval<const U &>()),
                                 decltype(std::declval<const T &>() != std::declval<const U &>()),
                                 decltype(std::declval<const T &>() < std::declval<const U &>()),
                                 decltype(std::declval<const T &>() <= std::declval<const U &>()),
                                 decltype(std::declval<const T &>() > std::declval<const U &>()),
                                 decltype(std::declval<const T &>() >= std::declval<const U &>()),
                                 decltype(std::declval<const U &>() == std::declval<const T &>()),
                                 decltype(std::declval<const U &>() != std::declval<const T &>()),
                                 decltype(std::declval<const U &>() < std::declval<const T &>()),
                                 decltype(std::declval<const U &>() <= std::declval<const T &>()),
                                 decltype(std::declval<const U &>() > std::declval<const T &>()),
                                 decltype(std::declval<const U &>() >= std::declval<const T &>())>> : std::true_type
{};

template <typename T, typename U>
inline constexpr bool is_comparable_v = is_comparable<T, U>::value;

// HasSet tells whether `Set(pixel)` can be called on the specified iterator type.
template <typename TIterator, typename = void>
struct HasSet : std::false_type
{};

template <typename TIterator>
struct HasSet<TIterator,
              std::void_t<decltype(std::declval<TIterator &>().Set(std::declval<typename TIterator::PixelType>()))>>
  : std::true_type
{};

// Checks that the writable iterator template provides Set() for a non-const image, and not for a const image.
template <template <typename> typename... TIteratorTemplate>
constexpr bool
AreWritableOnlyForNonConstImages()
{
  using ImageType = itk::Image<int>;
  return ((HasSet<TIteratorTemplate<ImageType>>::value && !HasSet<TIteratorTemplate<const ImageType>>::value) && ...);
}

// HasAssignableValue tells whether a pixel can be assigned through the reference returned by `Value()`.
template <typename TIterator, typename = void>
struct HasAssignableValue : std::false_type
{};

template <typename TIterator>
struct HasAssignableValue<
  TIterator,
  std::void_t<decltype(std::declval<TIterator &>().Value() = std::declval<typename TIterator::PixelType>())>>
  : std::true_type
{};

// Checks that `Value()` is assignable for a non-const image, and not for a const image.
template <template <typename> typename... TIteratorTemplate>
constexpr bool
AreValueAssignableOnlyForNonConstImages()
{
  using ImageType = itk::Image<int>;
  return ((HasAssignableValue<TIteratorTemplate<ImageType>>::value &&
           !HasAssignableValue<TIteratorTemplate<const ImageType>>::value) &&
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
  const itk::ImageRegionIterator<ImageType> iterator(image, image->GetBufferedRegion());

  const size_t numberOfCallsBefore = image->GetNumberOfNonConstGetBufferPointerCalls();
  [[maybe_unused]] const itk::ImageRegionReverseIterator<ImageType> reverseIterator(iterator);
  EXPECT_GT(image->GetNumberOfNonConstGetBufferPointerCalls(), numberOfCallsBefore);
}

// Checks conversion and comparison between const and non-const iterators
TEST(ImageIterators, ConstNonConstConversionAndComparison)
{
  using ImageType = itk::Image<int>;

  using MutableIt = itk::ImageIterator<ImageType>;
  using ConstIt = itk::ImageIterator<const ImageType>;
  using LegacyConstIt = itk::ImageConstIterator<ImageType>;

  static_assert(std::is_convertible_v<MutableIt, ConstIt>);
  static_assert(!std::is_convertible_v<ConstIt, MutableIt>);
  static_assert(std::is_convertible_v<LegacyConstIt, ConstIt>);
  static_assert(std::is_convertible_v<ConstIt, LegacyConstIt>);
  static_assert(std::is_convertible_v<MutableIt, LegacyConstIt>);
  static_assert(!std::is_constructible_v<MutableIt, ConstIt>);
  static_assert(!std::is_constructible_v<MutableIt, LegacyConstIt>);
  static_assert(!std::is_base_of_v<LegacyConstIt, MutableIt>);

  static_assert(!is_comparable_v<MutableIt, itk::ImageIterator<itk::Image<int, 3>>>);

  static_assert(is_comparable_v<MutableIt, ConstIt>);
  static_assert(is_comparable_v<MutableIt, LegacyConstIt>);
  static_assert(is_comparable_v<ConstIt, LegacyConstIt>);
}


// Checks that iterators over a const image provide no write access (issue #7003).
TEST(ImageIterators, NoWriteAccessToConstImage)
{
  static_assert(AreWritableOnlyForNonConstImages<itk::ImageIterator,
                                                 itk::ImageRegionIterator,
                                                 itk::ImageScanlineIterator,
                                                 itk::ImageReverseIterator,
                                                 itk::ImageRegionReverseIterator,
                                                 itk::ImageIteratorWithIndex,
                                                 itk::ImageRegionIteratorWithIndex,
                                                 itk::ImageLinearIteratorWithIndex,
                                                 itk::ImageSliceIteratorWithIndex,
                                                 itk::ImageRandomIteratorWithIndex,
                                                 itk::ImageRandomNonRepeatingIteratorWithIndex,
                                                 itk::ImageRegionExclusionIteratorWithIndex>());
  static_assert(!HasSet<itk::ImageRegionConstIterator<itk::Image<int>>>::value);
  static_assert(!HasSet<itk::ImageConstIterator<itk::Image<int>>>::value);
  static_assert(!HasSet<itk::ImageRegionConstIteratorWithIndex<itk::Image<int>>>::value);

  using ImageType = itk::Image<int>;
  static_assert(HasSet<itk::LineIterator<ImageType>>::value && !HasSet<itk::LineIterator<const ImageType>>::value);
  static_assert(!HasSet<itk::LineConstIterator<ImageType>>::value);

  static_assert(AreValueAssignableOnlyForNonConstImages<itk::ImageIterator,
                                                        itk::ImageRegionIterator,
                                                        itk::ImageScanlineIterator,
                                                        itk::ImageIteratorWithIndex,
                                                        itk::ImageRegionIteratorWithIndex,
                                                        itk::ImageLinearIteratorWithIndex,
                                                        itk::ImageSliceIteratorWithIndex,
                                                        itk::ImageRandomIteratorWithIndex,
                                                        itk::ImageRandomNonRepeatingIteratorWithIndex,
                                                        itk::ImageRegionExclusionIteratorWithIndex>());
  static_assert(!HasAssignableValue<itk::ImageRegionConstIterator<ImageType>>::value);
  static_assert(!HasAssignableValue<itk::ImageRegionConstIteratorWithIndex<ImageType>>::value);
}


// Checks that the indexed iterators keep the ImageIteratorDimension constant of the ITK 5 API.
TEST(ImageIterators, IndexedIteratorsHaveImageIteratorDimension)
{
  using ImageType = itk::Image<int, 3>;
  static_assert(itk::ImageIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
  static_assert(itk::ImageConstIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
  static_assert(itk::ImageRegionIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
  static_assert(itk::ImageRegionExclusionIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
}


// Checks that the exclusion iterator can still be constructed from a plain ImageIteratorWithIndex.
TEST(ImageIterators, ExclusionIteratorConstructibleFromImageIteratorWithIndex)
{
  using ImageType = itk::Image<int>;
  using IndexedIt = itk::ImageIteratorWithIndex<ImageType>;
  using ConstIndexedIt = itk::ImageIteratorWithIndex<const ImageType>;

  static_assert(std::is_convertible_v<IndexedIt, itk::ImageRegionExclusionIteratorWithIndex<ImageType>>);
  static_assert(std::is_constructible_v<itk::ImageRegionExclusionConstIteratorWithIndex<ImageType>, IndexedIt>);
  static_assert(std::is_constructible_v<itk::ImageRegionExclusionConstIteratorWithIndex<ImageType>, ConstIndexedIt>);
  static_assert(!std::is_constructible_v<itk::ImageRegionExclusionIteratorWithIndex<ImageType>, ConstIndexedIt>);
}


// Checks that the iterators deduce their image type from a raw image pointer, as they do in ITK 5.
TEST(ImageIterators, DeduceImageTypeFromRawPointer)
{
  using ImageType = itk::Image<int>;
  using RegionType = ImageType::RegionType;
  using IndexType = ImageType::IndexType;
  ImageType * const       image = nullptr;
  const ImageType * const constImage = nullptr;

  static_assert(
    std::is_same_v<decltype(itk::ImageRegionIterator(image, RegionType{})), itk::ImageRegionIterator<ImageType>>);
  static_assert(
    std::is_same_v<decltype(itk::ImageReverseIterator(image, RegionType{})), itk::ImageReverseIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageReverseIterator(constImage, RegionType{})),
                               itk::ImageReverseIterator<const ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageRegionReverseIterator(image, RegionType{})),
                               itk::ImageRegionReverseIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageRegionConstIterator(constImage, RegionType{})),
                               itk::ImageRegionConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageReverseConstIterator(constImage, RegionType{})),
                               itk::ImageReverseConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageRegionReverseConstIterator(constImage, RegionType{})),
                               itk::ImageRegionReverseConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::LineConstIterator(image, IndexType{}, IndexType{})),
                               itk::LineConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::LineConstIterator(constImage, IndexType{}, IndexType{})),
                               itk::LineConstIterator<ImageType>>);
}


// Checks that a copied random non-repeating iterator owns its permutation, so it outlives the source.
TEST(ImageIterators, RandomNonRepeatingCopyOutlivesSource)
{
  using ImageType = itk::Image<int>;
  const auto image = ImageType::New();
  const auto region = ImageType::RegionType(ImageType::SizeType::Filled(4));
  image->SetRegions(region);
  image->AllocateInitialized();

  const auto makeSource = [&image, &region] {
    auto source =
      std::make_unique<itk::ImageRandomNonRepeatingIteratorWithIndex<const ImageType>>(image.GetPointer(), region);
    source->SetNumberOfSamples(region.GetNumberOfPixels());
    source->GoToBegin();
    return source;
  };

  const auto expectUsable = [&region](auto & it) {
    for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      EXPECT_TRUE(region.IsInside(it.GetIndex()));
    }
  };

  auto source = makeSource();

  itk::ImageRandomNonRepeatingIteratorWithIndex<const ImageType> copy(*source);
  source = makeSource();

  itk::ImageRandomNonRepeatingConstIteratorWithIndex<ImageType> bridged(*source);
  source.reset();
  expectUsable(copy);
  expectUsable(bridged);
}
