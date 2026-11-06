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
#include <set>
#include <tuple>
#include <type_traits> // For false_type and true_type.
#include <typeinfo>
#include <vector>

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

template <typename T, typename U, typename = void>
struct is_equality_comparable : std::false_type
{};

template <typename T, typename U>
struct is_equality_comparable<T,
                              U,
                              std::void_t<decltype(std::declval<const T &>() == std::declval<const U &>()),
                                          decltype(std::declval<const T &>() != std::declval<const U &>()),
                                          decltype(std::declval<const U &>() == std::declval<const T &>()),
                                          decltype(std::declval<const U &>() != std::declval<const T &>())>>
  : std::true_type
{};

// The writable and read-only names of an iterator, over a 2D image.
template <template <typename> typename TIterator, template <typename> typename TConstIterator>
struct IteratorPair
{
  using ImageType = itk::Image<int>;
  using MutableIt = TIterator<ImageType>;
  using ConstIt = TConstIterator<ImageType>;
  using ReadOnlyIt = TIterator<const ImageType>;
  using OtherDimensionIt = TIterator<itk::Image<int, 3>>;
};

// Checks that writable iterators convert to read-only ones, and never the other way.
template <typename... TPair>
constexpr bool
HaveOneWayConversions(std::tuple<TPair...>)
{
  return ((std::is_convertible_v<typename TPair::MutableIt, typename TPair::ConstIt> &&
           !std::is_convertible_v<typename TPair::ConstIt, typename TPair::MutableIt> &&
           !std::is_constructible_v<typename TPair::MutableIt, typename TPair::ConstIt>) &&
          ...);
}

// Checks that iterators over the same image compare regardless of constness, and over another image do not.
template <template <typename, typename, typename> typename TComparable, typename... TPair>
constexpr bool
HaveConstNonConstComparisons(std::tuple<TPair...>)
{
  return ((!TComparable<typename TPair::MutableIt, typename TPair::OtherDimensionIt, void>::value &&
           TComparable<typename TPair::MutableIt, typename TPair::ConstIt, void>::value) &&
          ...);
}

// Checks that writable iterators convert to iterators over a const image, which convert to and from the
// read-only name, and that no writable iterator derives from or is built from a read-only one.
template <typename... TPair>
constexpr bool
HaveReadOnlyConversions(std::tuple<TPair...>)
{
  return ((std::is_convertible_v<typename TPair::MutableIt, typename TPair::ReadOnlyIt> &&
           !std::is_convertible_v<typename TPair::ReadOnlyIt, typename TPair::MutableIt> &&
           std::is_convertible_v<typename TPair::ConstIt, typename TPair::ReadOnlyIt> &&
           std::is_convertible_v<typename TPair::ReadOnlyIt, typename TPair::ConstIt> &&
           !std::is_constructible_v<typename TPair::MutableIt, typename TPair::ReadOnlyIt> &&
           !std::is_base_of_v<typename TPair::ConstIt, typename TPair::MutableIt>) &&
          ...);
}

// Checks that iterators over a const image compare with the writable and read-only names.
template <template <typename, typename, typename> typename TComparable, typename... TPair>
constexpr bool
HaveReadOnlyComparisons(std::tuple<TPair...>)
{
  return ((TComparable<typename TPair::MutableIt, typename TPair::ReadOnlyIt, void>::value &&
           TComparable<typename TPair::ReadOnlyIt, typename TPair::ConstIt, void>::value) &&
          ...);
}

template <typename TMutableIt, typename... TPair>
constexpr bool
IsNotConstructibleFromReadOnlyIterators()
{
  return ((!std::is_constructible_v<TMutableIt, typename TPair::ReadOnlyIt> &&
           !std::is_constructible_v<TMutableIt, typename TPair::ConstIt>) &&
          ...);
}

// Checks that no writable iterator is constructible from a read-only iterator of any of the pairs.
template <typename... TPair>
constexpr bool
NoWritableIteratorFromReadOnlyIterator(std::tuple<TPair...>)
{
  return (IsNotConstructibleFromReadOnlyIterators<typename TPair::MutableIt, TPair...>() && ...);
}

// HasSet tells whether `Set(pixel)` can be called on the specified iterator type.
template <typename TIterator, typename = void>
struct HasSet : std::false_type
{};

template <typename TIterator>
struct HasSet<TIterator,
              std::void_t<decltype(std::declval<TIterator &>().Set(std::declval<typename TIterator::PixelType>()))>>
  : std::true_type
{};

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

// Checks that the writable name of each pair can write pixels, and the read-only name cannot.
template <typename... TPair>
constexpr bool
OnlyWritableNamesWrite(std::tuple<TPair...>)
{
  return ((HasSet<typename TPair::MutableIt>::value && !HasSet<typename TPair::ConstIt>::value &&
           HasAssignableValue<typename TPair::MutableIt>::value &&
           !HasAssignableValue<typename TPair::ConstIt>::value) &&
          ...);
}

using ForwardIteratorPairs = std::tuple<
  IteratorPair<itk::ImageIterator, itk::ImageConstIterator>,
  IteratorPair<itk::ImageRegionIterator, itk::ImageRegionConstIterator>,
  IteratorPair<itk::ImageScanlineIterator, itk::ImageScanlineConstIterator>,
  IteratorPair<itk::ImageIteratorWithIndex, itk::ImageConstIteratorWithIndex>,
  IteratorPair<itk::ImageRegionIteratorWithIndex, itk::ImageRegionConstIteratorWithIndex>,
  IteratorPair<itk::ImageLinearIteratorWithIndex, itk::ImageLinearConstIteratorWithIndex>,
  IteratorPair<itk::ImageSliceIteratorWithIndex, itk::ImageSliceConstIteratorWithIndex>,
  IteratorPair<itk::ImageRandomIteratorWithIndex, itk::ImageRandomConstIteratorWithIndex>,
  IteratorPair<itk::ImageRandomNonRepeatingIteratorWithIndex, itk::ImageRandomNonRepeatingConstIteratorWithIndex>,
  IteratorPair<itk::ImageRegionExclusionIteratorWithIndex, itk::ImageRegionExclusionConstIteratorWithIndex>>;
using ReverseIteratorPairs =
  std::tuple<IteratorPair<itk::ImageReverseIterator, itk::ImageReverseConstIterator>,
             IteratorPair<itk::ImageRegionReverseIterator, itk::ImageRegionReverseConstIterator>>;
using LinePair = IteratorPair<itk::LineIterator, itk::LineConstIterator>;
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


// Checks that ImageRegionReverseIterator(const ImageIterator &) calls the non-const GetBufferPointer() overload.
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


// Checks that a reverse region iterator placed in the middle of a region walks only the remaining region pixels.
TEST(ImageIterators, ReverseRegionIteratorPlacedMidRegionStaysInRegion)
{
  using ImageType = itk::Image<int>;
  using IndexType = ImageType::IndexType;
  using ReverseIteratorType = itk::ImageRegionReverseConstIterator<ImageType>;
  const auto image = ImageType::New();
  image->SetRegions(itk::MakeSize(10, 4));
  image->AllocateInitialized();

  const ImageType::RegionType  region(itk::MakeIndex(2, 1), itk::MakeSize(3, 2));
  const std::vector<IndexType> regionInReverseOrder{ itk::MakeIndex(4, 2), itk::MakeIndex(3, 2), itk::MakeIndex(2, 2),
                                                     itk::MakeIndex(4, 1), itk::MakeIndex(3, 1), itk::MakeIndex(2, 1) };

  const auto remainingIndices = [&region](ReverseIteratorType iterator) {
    std::vector<IndexType> indices;
    for (; !iterator.IsAtEnd() && indices.size() <= region.GetNumberOfPixels(); ++iterator)
    {
      indices.push_back(iterator.GetIndex());
    }
    return indices;
  };

  // Start at the row's last pixel, then mid-row.
  for (auto startIt = regionInReverseOrder.cbegin(); startIt != regionInReverseOrder.cbegin() + 2; ++startIt)
  {
    const IndexType              start = *startIt;
    const std::vector<IndexType> expected(startIt, regionInReverseOrder.cend());

    ReverseIteratorType placedIt(image, region);
    placedIt.SetIndex(start);
    EXPECT_EQ(remainingIndices(placedIt), expected) << "start " << start;

    itk::ImageRegionIterator<ImageType> forwardIt(image, region);
    forwardIt.SetIndex(start);
    EXPECT_EQ(remainingIndices(ReverseIteratorType(forwardIt)), expected) << "start " << start;
  }
}


// Checks that copy-assigning a non-repeating random iterator copies the order in which it visits pixels.
TEST(ImageIterators, RandomNonRepeatingCopyAssignmentKeepsVisitOrder)
{
  using ImageType = itk::Image<int>;
  using IteratorType = itk::ImageRandomNonRepeatingConstIteratorWithIndex<ImageType>;
  const auto image = ImageType::New();
  image->SetRegions(itk::MakeSize(4, 4));
  image->AllocateInitialized();
  const auto region = image->GetBufferedRegion();

  const auto visitedIndices = [](IteratorType & iterator) {
    std::vector<ImageType::IndexType> indices;
    for (iterator.GoToBegin(); !iterator.IsAtEnd(); ++iterator)
    {
      indices.push_back(iterator.GetIndex());
    }
    return indices;
  };

  IteratorType original(image, region);
  original.SetNumberOfSamples(region.GetNumberOfPixels());
  original.ReinitializeSeed(7);

  IteratorType assigned(image, region);
  assigned.SetNumberOfSamples(region.GetNumberOfPixels());
  assigned = original;

  const auto expected = visitedIndices(original);
  EXPECT_EQ(visitedIndices(assigned), expected);
  EXPECT_EQ(std::set<ImageType::IndexType>(expected.cbegin(), expected.cend()).size(), expected.size());

  // The number of samples comes from the source, whatever the target held before.
  IteratorType defaultConstructed;
  defaultConstructed = original;
  EXPECT_EQ(visitedIndices(defaultConstructed), expected);

  IteratorType overSmallerRegion(image, ImageType::RegionType(itk::MakeSize(2, 2)));
  overSmallerRegion.SetNumberOfSamples(1);
  overSmallerRegion = original;
  EXPECT_EQ(visitedIndices(overSmallerRegion), expected);
}


// Checks that a copied permutation keeps the order and owns its random number generator.
TEST(ImageIterators, RandomPermutationCopyIsIndependent)
{
  static constexpr itk::SizeValueType size = 16;

  const auto order = [](const itk::RandomPermutation & permutation) {
    std::vector<itk::SizeValueType> indices;
    for (itk::SizeValueType i = 0; i < size; ++i)
    {
      indices.push_back(permutation[i]);
    }
    return indices;
  };

  itk::RandomPermutation original(size);
  original.ReinitializeSeed(1);
  original.Shuffle();

  const itk::RandomPermutation copied(original);
  EXPECT_EQ(order(copied), order(original));

  itk::RandomPermutation assigned(size / 2);
  assigned = original;
  EXPECT_EQ(order(assigned), order(original));

  copied.ReinitializeSeed(2);
  assigned.ReinitializeSeed(3);
  original.Shuffle();

  itk::RandomPermutation reference(size);
  reference.ReinitializeSeed(1);
  reference.Shuffle();
  reference.Shuffle();
  EXPECT_EQ(order(original), order(reference));

  EXPECT_THROW(original.SetPriority(size, 0), std::runtime_error);
  EXPECT_NO_THROW(original.SetPriority(size - 1, 0));
}


// Checks that a copy of a non-repeating random iterator still visits every pixel once after the source is destroyed.
TEST(ImageIterators, RandomNonRepeatingCopyOutlivesSource)
{
  using ImageType = itk::Image<int>;
  using IteratorType = itk::ImageRandomNonRepeatingConstIteratorWithIndex<ImageType>;
  const auto image = ImageType::New();
  const auto region = ImageType::RegionType(ImageType::SizeType::Filled(4));
  image->SetRegions(region);
  image->AllocateInitialized();

  auto source = std::make_unique<IteratorType>(image, region);
  source->SetNumberOfSamples(region.GetNumberOfPixels());
  source->GoToBegin();

  IteratorType copy(*source);
  source.reset();

  std::set<itk::OffsetValueType> visitedOffsets;
  itk::SizeValueType             numberOfVisits{};
  for (copy.GoToBegin(); !copy.IsAtEnd(); ++copy)
  {
    EXPECT_TRUE(region.IsInside(copy.GetIndex()));
    visitedOffsets.insert(image->ComputeOffset(copy.GetIndex()));
    ++numberOfVisits;
  }
  EXPECT_EQ(numberOfVisits, region.GetNumberOfPixels());
  EXPECT_EQ(visitedOffsets.size(), region.GetNumberOfPixels());
}


// Checks that writable iterators convert to, and compare with, their read-only counterparts.
TEST(ImageIterators, WritableToReadOnlyConversionAndComparison)
{
  static_assert(HaveOneWayConversions(std::tuple_cat(ForwardIteratorPairs{}, ReverseIteratorPairs{})));
  static_assert(HaveOneWayConversions(std::tuple<LinePair>{}));
  static_assert(HaveConstNonConstComparisons<is_comparable>(ForwardIteratorPairs{}));
  static_assert(HaveConstNonConstComparisons<is_equality_comparable>(ReverseIteratorPairs{}));
}


// Checks conversion and comparison between iterators over a const image and the other names.
TEST(ImageIterators, ConstNonConstConversionAndComparison)
{
  using ImageIteratorPairs = decltype(std::tuple_cat(ForwardIteratorPairs{}, ReverseIteratorPairs{}));
  static_assert(HaveReadOnlyConversions(ImageIteratorPairs{}));
  static_assert(HaveReadOnlyConversions(std::tuple<LinePair>{}));
  static_assert(HaveReadOnlyComparisons<is_comparable>(ForwardIteratorPairs{}));
  static_assert(HaveReadOnlyComparisons<is_equality_comparable>(ImageIteratorPairs{}));
  static_assert(NoWritableIteratorFromReadOnlyIterator(std::tuple_cat(ImageIteratorPairs{}, std::tuple<LinePair>{})));
}


// Checks that only the writable iterator names provide Set() and an assignable Value().
TEST(ImageIterators, OnlyWritableIteratorsWrite)
{
  static_assert(OnlyWritableNamesWrite(ForwardIteratorPairs{}));
  using ImageType = itk::Image<int>;
  static_assert(HasSet<itk::ImageReverseIterator<ImageType>>::value);
  static_assert(HasSet<itk::ImageRegionReverseIterator<ImageType>>::value);
  static_assert(HasSet<itk::LineIterator<ImageType>>::value && !HasSet<itk::LineConstIterator<ImageType>>::value);
}


// Checks that the indexed iterators have the ImageIteratorDimension constant.
TEST(ImageIterators, IndexedIteratorsHaveImageIteratorDimension)
{
  using ImageType = itk::Image<int, 3>;
  static_assert(itk::ImageIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
  static_assert(itk::ImageConstIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
  static_assert(itk::ImageRegionIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
  static_assert(itk::ImageRegionExclusionIteratorWithIndex<ImageType>::ImageIteratorDimension == 3);
}


// Checks that the exclusion iterators can be constructed from the matching indexed iterator.
TEST(ImageIterators, ExclusionIteratorConstructibleFromImageIteratorWithIndex)
{
  using ImageType = itk::Image<int>;
  using IndexedIt = itk::ImageIteratorWithIndex<ImageType>;
  using ConstIndexedIt = itk::ImageConstIteratorWithIndex<ImageType>;

  static_assert(std::is_convertible_v<IndexedIt, itk::ImageRegionExclusionIteratorWithIndex<ImageType>>);
  static_assert(std::is_constructible_v<itk::ImageRegionExclusionConstIteratorWithIndex<ImageType>, IndexedIt>);
  static_assert(std::is_constructible_v<itk::ImageRegionExclusionConstIteratorWithIndex<ImageType>, ConstIndexedIt>);
  static_assert(!std::is_constructible_v<itk::ImageRegionExclusionIteratorWithIndex<ImageType>, ConstIndexedIt>);

  using ReadOnlyIndexedIt = itk::ImageIteratorWithIndex<const ImageType>;
  static_assert(std::is_constructible_v<itk::ImageRegionExclusionConstIteratorWithIndex<ImageType>, ReadOnlyIndexedIt>);
  static_assert(!std::is_constructible_v<itk::ImageRegionExclusionIteratorWithIndex<ImageType>, ReadOnlyIndexedIt>);
}


// Checks that the iterators deduce their image type from a raw image pointer.
TEST(ImageIterators, DeduceImageTypeFromRawPointer)
{
  using ImageType = itk::Image<int>;
  using RegionType = ImageType::RegionType;
  ImageType * const       image = nullptr;
  const ImageType * const constImage = nullptr;

  static_assert(
    std::is_same_v<decltype(itk::ImageRegionIterator(image, RegionType{})), itk::ImageRegionIterator<ImageType>>);
  static_assert(
    std::is_same_v<decltype(itk::ImageReverseIterator(image, RegionType{})), itk::ImageReverseIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageRegionReverseIterator(image, RegionType{})),
                               itk::ImageRegionReverseIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageRegionConstIterator(constImage, RegionType{})),
                               itk::ImageRegionConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageReverseConstIterator(constImage, RegionType{})),
                               itk::ImageReverseConstIterator<ImageType>>);
  static_assert(std::is_same_v<decltype(itk::ImageRegionReverseConstIterator(constImage, RegionType{})),
                               itk::ImageRegionReverseConstIterator<ImageType>>);
}


// Checks that a read-only iterator converted from a writable random non-repeating one owns its permutation.
TEST(ImageIterators, RandomNonRepeatingConversionOutlivesSource)
{
  using ImageType = itk::Image<int>;
  const auto image = ImageType::New();
  const auto region = ImageType::RegionType(ImageType::SizeType::Filled(4));
  image->SetRegions(region);
  image->AllocateInitialized();

  auto source = std::make_unique<itk::ImageRandomNonRepeatingIteratorWithIndex<ImageType>>(image.GetPointer(), region);
  source->SetNumberOfSamples(region.GetNumberOfPixels());
  source->ReinitializeSeed(7);
  std::vector<ImageType::IndexType> expected;
  for (source->GoToBegin(); !source->IsAtEnd(); ++(*source))
  {
    expected.push_back(source->GetIndex());
  }

  itk::ImageRandomNonRepeatingConstIteratorWithIndex<ImageType> converted(*source);
  source.reset();

  std::vector<ImageType::IndexType> visited;
  for (converted.GoToBegin(); !converted.IsAtEnd(); ++converted)
  {
    visited.push_back(converted.GetIndex());
  }
  EXPECT_EQ(visited, expected);
  EXPECT_EQ(expected.size(), region.GetNumberOfPixels());
}


// Checks that the incrementable reverse iterators visit the pixels of a region in reverse order.
TEST(ImageIterators, ReverseIteratorsWalkRegionBackward)
{
  using ImageType = itk::Image<int>;
  const auto image = ImageType::New();
  image->SetRegions(itk::MakeSize(5, 4));
  image->Allocate();
  int value{};
  for (itk::ImageRegionIterator<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    it.Set(value++);
  }
  const ImageType::RegionType region(itk::MakeIndex(1, 1), itk::MakeSize(3, 2));

  std::vector<int> expected;
  for (itk::ImageRegionConstIterator<ImageType> it(image, region); !it.IsAtEnd(); ++it)
  {
    expected.insert(expected.begin(), it.Get());
  }

  const auto visit = [](auto it) {
    std::vector<int> values;
    for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      values.push_back(it.Get());
    }
    return values;
  };
  EXPECT_EQ(visit(itk::ImageReverseIterator<ImageType>(image, region)), expected);
  EXPECT_EQ(visit(itk::ImageRegionReverseIterator<ImageType>(image, region)), expected);
  EXPECT_EQ(visit(itk::ImageRegionReverseConstIterator<ImageType>(image, region)), expected);
}
