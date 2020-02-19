/*=========================================================================
 *
 *  Copyright NumFOCUS
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

// First include the header file to be tested:
#include "itkIndexRange.h"

#include "itkRangeGTestUtilities.h"

#include <gtest/gtest.h>

// Test template instantiations for various template arguments:
template class itk::Experimental::IndexRange<1, true>;
template class itk::Experimental::IndexRange<1, false>;
template class itk::Experimental::IndexRange<2, true>;
template class itk::Experimental::IndexRange<2, false>;


using itk::Experimental::IndexRange;
using itk::Experimental::ImageRegionIndexRange;
using itk::Experimental::ZeroBasedIndexRange;
using itk::Experimental::RangeGTestUtilities;


static_assert(sizeof(ZeroBasedIndexRange<3>) < sizeof(ImageRegionIndexRange<3>),
              "ZeroBasedIndexRange does not need to store the index of a region, so it should take less memory.");

namespace
{
template <unsigned VDimension>
itk::Index<VDimension>
GenerateRandomIndex()
{
  itk::Index<VDimension> index;

  for (itk::IndexValueType & indexValue : index)
  {
    indexValue = std::rand();
  }
  return index;
}


template <bool VBeginAtZero>
void
ExpectBeginIsEndWhenRangeIsDefaultConstructed()
{
  // Test the most commonly used dimensionalities (1-D, 2-D, 3-D):
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<IndexRange<1, VBeginAtZero>>();
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<IndexRange<2, VBeginAtZero>>();
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<IndexRange<3, VBeginAtZero>>();
}


template <bool VBeginAtZero>
void
ExpectZeroSizeWhenRangeIsDefaultConstructed()
{
  // Test the most commonly used dimensionalities (1-D, 2-D, 3-D):
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<IndexRange<1, VBeginAtZero>>();
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<IndexRange<2, VBeginAtZero>>();
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<IndexRange<3, VBeginAtZero>>();
}


template <bool VBeginAtZero>
void
ExpectRangeIsEmptyWhenDefaultConstructed()
{
  // Test the most commonly used dimensionalities (1-D, 2-D, 3-D):
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<IndexRange<1, VBeginAtZero>>();
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<IndexRange<2, VBeginAtZero>>();
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<IndexRange<3, VBeginAtZero>>();
}

template <unsigned VDimension>
void
ExpectRangeIsEmptyWhenRegionSizeIsZero()
{
  const itk::Size<VDimension> zeroSize{ { 0 } };

  // Test when both the region index and the region size are zero:
  EXPECT_TRUE(ZeroBasedIndexRange<VDimension>{ zeroSize }.empty());
  EXPECT_TRUE(ImageRegionIndexRange<VDimension>{ zeroSize }.empty());

  // Now do the test for an arbitrary (random) region index:
  const itk::Index<VDimension>       randomRegionIndex = GenerateRandomIndex<VDimension>();
  const itk::ImageRegion<VDimension> zeroSizedImageRegion{ randomRegionIndex, zeroSize };

  EXPECT_TRUE(ImageRegionIndexRange<VDimension>{ zeroSizedImageRegion }.empty());
}


template <typename TRange>
void
ExpectRangeBeginIsEnd(const TRange & range)
{
  EXPECT_EQ(range.cbegin(), range.cend());
}


template <unsigned VDimension>
void
ExpectRangeBeginIsEndWhenSizeHasZeroValue()
{
  const itk::Index<VDimension> randomIndex = GenerateRandomIndex<VDimension>();

  for (unsigned i{}; i < VDimension; ++i)
  {
    auto size = itk::Size<VDimension>::Filled(2);

    size[i] = 0;

    ExpectRangeBeginIsEnd(ZeroBasedIndexRange<VDimension>{ size });
    ExpectRangeBeginIsEnd(ImageRegionIndexRange<VDimension>{ size });

    // Now do the test for an arbitrary (random) region index:
    const itk::ImageRegion<VDimension> imageRegion{ randomIndex, size };

    ExpectRangeBeginIsEnd(ImageRegionIndexRange<VDimension>{ imageRegion });
  }
}
} // namespace


// Tests that a begin iterator compares equal to another begin iterator of the
// same range. Also does this test for end iterators.
TEST(IndexRange, EquivalentBeginOrEndIteratorsCompareEqual)
{
  using RangeType = IndexRange<2, true>;

  RangeType range(RangeType::SizeType{ { 1, 2 } });

  const RangeType::iterator       begin = range.begin();
  const RangeType::iterator       end = range.end();
  const RangeType::const_iterator cbegin = range.cbegin();
  const RangeType::const_iterator cend = range.cend();

  // An iterator object compares equal to itself:
  EXPECT_EQ(begin, begin);
  EXPECT_EQ(end, end);
  EXPECT_EQ(cbegin, cbegin);
  EXPECT_EQ(cend, cend);

  // Multiple calls of the same function yield equivalent objects:
  EXPECT_EQ(range.begin(), range.begin());
  EXPECT_EQ(range.end(), range.end());
  EXPECT_EQ(range.cbegin(), range.cbegin());
  EXPECT_EQ(range.cend(), range.cend());

  // Corresponding const_iterator and non-const iterator compare equal:
  EXPECT_EQ(begin, cbegin);
  EXPECT_EQ(end, cend);
  EXPECT_EQ(cbegin, begin);
  EXPECT_EQ(cend, end);
}


// Tests that a 'begin' iterator and an 'end' iterator do not compare
// equal, when the size of the range is greater than zero.
TEST(IndexRange, BeginAndEndDoNotCompareEqualWhenSizeIsGreaterThanZero)
{
  using RangeType = IndexRange<2, true>;
  RangeType range(RangeType::SizeType{ { 1, 2 } });

  EXPECT_TRUE(!range.empty());
  EXPECT_FALSE(range.empty());

  EXPECT_FALSE(range.begin() == range.end());
  EXPECT_NE(range.begin(), range.end());
}


// Tests that the iterators of an IndexRange can be used as first and
// second argument of an std::vector constructor.
TEST(IndexRange, IteratorsCanBePassedToStdVectorConstructor)
{
  using RangeType = IndexRange<2, true>;
  RangeType range(RangeType::SizeType{ { 1, 2 } });

  // Easily store all indices of an IndexRange in an std::vector:
  const std::vector<RangeType::IndexType> stdVector(range.begin(), range.end());
  EXPECT_EQ(stdVector, std::vector<RangeType::IndexType>(range.cbegin(), range.cend()));
  ASSERT_EQ(stdVector.size(), range.size());
  EXPECT_TRUE(std::equal(stdVector.cbegin(), stdVector.cend(), range.cbegin()));
}


// Tests that the iterators of an IndexRange can be used as first and
// second argument of std::reverse (which requires bidirectional iterators).
TEST(IndexRange, IteratorsCanBePassedToStdReverseCopy)
{
  using RangeType = IndexRange<2, true>;
  using IndexType = RangeType::IndexType;
  RangeType range(RangeType::SizeType{ { 2, 3 } });

  const unsigned numberOfIndices = range.size();

  const std::vector<IndexType> stdVector(range.begin(), range.end());
  std::vector<IndexType>       reversedStdVector1(numberOfIndices);
  std::vector<IndexType>       reversedStdVector2(numberOfIndices);
  std::vector<IndexType>       reversedStdVector3(numberOfIndices);

  // Checks bidirectionality of the range iterators.
  std::reverse_copy(stdVector.cbegin(), stdVector.cend(), reversedStdVector1.begin());
  std::reverse_copy(range.begin(), range.end(), reversedStdVector2.begin());
  std::reverse_copy(range.cbegin(), range.cend(), reversedStdVector3.begin());

  // Sanity check
  EXPECT_NE(reversedStdVector1, stdVector);
  EXPECT_NE(reversedStdVector2, stdVector);
  EXPECT_NE(reversedStdVector3, stdVector);

  // The real tests:
  EXPECT_EQ(reversedStdVector1, reversedStdVector2);
  EXPECT_EQ(reversedStdVector1, reversedStdVector3);
}


// Tests that the iterators of an IndexRange can be used as first and
// second argument of std::for_each.
TEST(IndexRange, IteratorsCanBePassedToStdForEach)
{
  using RangeType = IndexRange<2, true>;
  using IndexType = RangeType::IndexType;
  RangeType range(RangeType::SizeType{ { 2, 3 } });

  std::for_each(range.begin(), range.end(), [](const IndexType index) { EXPECT_TRUE(index >= IndexType()); });
}


// Tests that an IndexRange can be used as the "range expression" of a
// C++11 range-based for loop.
TEST(IndexRange, CanBeUsedAsExpressionOfRangeBasedForLoop)
{
  using RangeType = IndexRange<2, true>;
  using IndexType = RangeType::IndexType;
  RangeType range(RangeType::SizeType{ { 2, 3 } });

  for (auto && index : range)
  {
    EXPECT_TRUE(index >= IndexType());
  }
}


TEST(IndexRange, SupportsImageRegion)
{
  constexpr unsigned Dimension = 2;

  using ImageRegionIndexRangeType = ImageRegionIndexRange<Dimension>;
  using IndexType = ImageRegionIndexRangeType::IndexType;
  using RegionType = itk::ImageRegion<Dimension>;

  const RegionType::SizeType  size = { { 2, 3 } };
  const RegionType::IndexType regionIndex = { { 4, 5 } };
  const RegionType            imageRegion{ regionIndex, size };

  // Default index range, beginning at [0, 0].
  const ZeroBasedIndexRange<Dimension> indexRange(size);
  const auto                           beginOfIndexRange = indexRange.cbegin();
  const auto                           endOfIndexRange = indexRange.cend();

  const ImageRegionIndexRangeType imageRegionIndexRange(imageRegion);

  EXPECT_EQ(imageRegionIndexRange.size(), indexRange.size());

  const auto offset = regionIndex - IndexType();
  auto       indexIterator = beginOfIndexRange;

  for (auto && imageRegionIndex : imageRegionIndexRange)
  {
    // Emergency stop if 'indexIterator' would already be at the end.
    ASSERT_NE(indexIterator, endOfIndexRange);

    EXPECT_EQ(imageRegionIndex, *indexIterator + offset);
    ++indexIterator;
  }
}


// Tests that begin() == end() for a default-constructed range.
TEST(IndexRange, BeginIsEndWhenDefaultConstructed)
{
  ExpectBeginIsEndWhenRangeIsDefaultConstructed<false>();
  ExpectBeginIsEndWhenRangeIsDefaultConstructed<true>();
}


// Tests that size() returns 0 for a default-constructed range.
TEST(IndexRange, SizeIsZeroWhenDefaultConstructed)
{
  ExpectZeroSizeWhenRangeIsDefaultConstructed<false>();
  ExpectZeroSizeWhenRangeIsDefaultConstructed<true>();
}


// Tests empty() for a default-constructed range.
TEST(IndexRange, IsEmptyWhenDefaultConstructed)
{
  ExpectRangeIsEmptyWhenDefaultConstructed<false>();
  ExpectRangeIsEmptyWhenDefaultConstructed<true>();
}


// Tests empty() for a range that has a region size of zero.
TEST(IndexRange, IsEmptyWhenRegionSizeIsZero)
{
  ExpectRangeIsEmptyWhenRegionSizeIsZero<1>();
  ExpectRangeIsEmptyWhenRegionSizeIsZero<2>();
  ExpectRangeIsEmptyWhenRegionSizeIsZero<3>();
}


// Tests that the begin is equal to the end, for a range constructed with an itk::Size
// that has a size value of zero.
TEST(IndexRange, BeginIsEndWhenSizeHasZeroValue)
{
  ExpectRangeBeginIsEndWhenSizeHasZeroValue<2>();
  ExpectRangeBeginIsEndWhenSizeHasZeroValue<3>();
}
