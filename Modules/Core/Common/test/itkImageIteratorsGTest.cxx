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
#include <memory>
#include <set>
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
  const itk::ImageRegionConstIterator<ImageType> constIterator(image, image->GetBufferedRegion());

  const size_t numberOfCallsBefore = image->GetNumberOfNonConstGetBufferPointerCalls();
  [[maybe_unused]] const itk::ImageRegionReverseIterator<ImageType> reverseIterator(constIterator);
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
