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

#ifndef itkRangeGTestUtilities_h
#define itkRangeGTestUtilities_h

#include <gtest/gtest.h> // For EXPECT_EQ.

#include <utility>  // For move.
#include <iterator> // For begin and end.

namespace itk
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


  template <typename TRange>
  static void
  ExpectIteratorIsDefaultConstructible()
  {
    using IteratorType = typename TRange::iterator;
    IteratorType defaultConstructedIterator{};

    // Test that a default-constructed iterator behaves according to C++ proposal
    // N3644, "Null Forward Iterators" by Alan Talbot, which is accepted with
    // C++14: "value-initialized iterators may be compared and shall compare
    // equal to other value-initialized iterators of the same type."
    // https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3644.pdf

    ExpectIteratorEqualsItself(defaultConstructedIterator);
    EXPECT_EQ(defaultConstructedIterator, IteratorType());
  }


  // Checks the `constexpr` member functions begin() and end() of a container like FixedArray, Index, Offset and Size.
  template <typename TContainer>
  static constexpr bool
  CheckConstexprBeginAndEndOfContainer()
  {
    using ConstContainerType = const TContainer;
    using ValueType = std::remove_reference_t<decltype(*(TContainer().begin()))>;

    static_assert(std::is_same<decltype(*(TContainer().begin())), ValueType &>::value,
                  "For a non-const container, begin() should return a non-const reference");
    static_assert(std::is_same<decltype(*(ConstContainerType().begin())), const ValueType &>::value,
                  "For a const container, begin() should return a const reference");
    static_assert(std::is_same<decltype(*(TContainer().cbegin())), const ValueType &>::value &&
                    std::is_same<decltype(*(ConstContainerType().cbegin())), const ValueType &>::value,
                  "For any container, cbegin() should return a const reference");

    static_assert(std::is_same<decltype(*(TContainer().end())), ValueType &>::value,
                  "For a non-const container, end() should return a non-const reference");
    static_assert(std::is_same<decltype(*(ConstContainerType().end())), const ValueType &>::value,
                  "For a const container, end() should return a const reference");
    static_assert(std::is_same<decltype(*(TContainer().cend())), const ValueType &>::value &&
                    std::is_same<decltype(*(ConstContainerType().cend())), const ValueType &>::value,
                  "For any container, cend() should return a const reference");

    constexpr TContainer container{};

    static_assert(container.cbegin() == container.begin(), "cbegin() should return the same iterator as begin().");
    static_assert(container.cend() == container.end(), "cend() should return the same iterator as end().");

    // Just return true to ease calling this function inside a static_assert.
    return true;
  }

private:
  template <typename TRange>
  static void
  ExpectRangesHaveEqualBeginAndEnd(const TRange & range1, const TRange & range2)
  {
    EXPECT_EQ(std::begin(range1), std::begin(range2));
    EXPECT_EQ(std::end(range1), std::end(range2));
  }

  template <typename TIterator>
  static void
  ExpectIteratorEqualsItself(const TIterator & it)
  {
    static_assert(!std::is_pointer<TIterator>::value,
                  "There should be a specific `ExpectIteratorEqualsItself` overload for a pointer as argument");

    // Checks the (typically) user-defined `operator==` and `operator!=` of TIterator.
    EXPECT_TRUE(it == it);
    EXPECT_FALSE(it != it);
  }

  template <typename TValue>
  static void
  ExpectIteratorEqualsItself(TValue *)
  {
    // Overload for the use of a pointer type as iterator. Intentionally does "nothing", as a pointer always
    // equals itself, by definition. Aims to avoid "warning: self-comparison always evaluates to false/true
    // [-Wtautological-compare]", as produced by clang 13.0.1 on Ubuntu 20.04.
  }
};

} // end namespace itk

#endif
