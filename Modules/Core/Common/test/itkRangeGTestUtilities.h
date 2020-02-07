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

#ifndef itkRangeGTestUtilities_h
#define itkRangeGTestUtilities_h

#include <gtest/gtest.h> // For EXPECT_EQ.

#include <utility>  // For move.
#include <iterator> // For begin and end.

namespace itk
{
namespace Experimental
{
// Utilities for GoogleTest unit tests of iterator ranges.
// Note: This class is only for internal (testing) purposes.
// It is not part of the public API of ITK.
class RangeGTestUtilities
{
public:
  template <typename TRange>
  static void
  ExpectBeginIsEndWhenRangeIsDefaultConstructed()
  {
    TRange defaultConstructedRange;
    EXPECT_EQ(std::begin(defaultConstructedRange), std::end(defaultConstructedRange));
  }


  template <typename TRange>
  static void
  ExpectZeroSizeWhenRangeIsDefaultConstructed()
  {
    TRange defaultConstructedRange;
    EXPECT_EQ(defaultConstructedRange.size(), 0);
  }


  template <typename TRange>
  static void
  ExpectRangeIsEmptyWhenDefaultConstructed()
  {
    TRange defaultConstructedRange;
    EXPECT_TRUE(defaultConstructedRange.empty());
  }


  template <typename TRange>
  static void
  ExpectCopyConstructedRangeHasSameIteratorsAsOriginal(const TRange & originalRange)
  {
    const TRange copyConstructedRange(originalRange);

    ExpectRangesHaveEqualBeginAndEnd(copyConstructedRange, originalRange);
  }


  template <typename TRange>
  static void
  ExpectCopyAssignedRangeHasSameIteratorsAsOriginal(const TRange & originalRange)
  {
    TRange copyAssignedRange;
    copyAssignedRange = originalRange;

    ExpectRangesHaveEqualBeginAndEnd(copyAssignedRange, originalRange);
  }


  template <typename TRange>
  static void
  ExpectMoveConstructedRangeHasSameIteratorsAsOriginalBeforeMove(TRange && originalRange)
  {
    const TRange originalRangeBeforeMove = originalRange;
    TRange       moveConstructedRange(std::move(originalRange));

    ExpectRangesHaveEqualBeginAndEnd(moveConstructedRange, originalRangeBeforeMove);
  }


  template <typename TRange>
  static void
  ExpectMoveAssignedRangeHasSameIteratorsAsOriginalBeforeMove(TRange && originalRange)
  {
    const TRange originalRangeBeforeMove = originalRange;

    TRange moveAssignedRange;
    moveAssignedRange = std::move(originalRange);

    ExpectRangesHaveEqualBeginAndEnd(moveAssignedRange, originalRangeBeforeMove);
  }

private:
  template <typename TRange>
  static void
  ExpectRangesHaveEqualBeginAndEnd(const TRange & range1, const TRange & range2)
  {
    EXPECT_EQ(std::begin(range1), std::begin(range2));
    EXPECT_EQ(std::end(range1), std::end(range2));
  }
};

} // end namespace Experimental
} // end namespace itk

#endif
