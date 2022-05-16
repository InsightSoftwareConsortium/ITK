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

// Enable testing legacy member functions rBegin() and rEnd().
#define ITK_LEGACY_TEST

// First include the header file to be tested:
#include "itkFixedArray.h"
#include "itkRangeGTestUtilities.h"
#include <gtest/gtest.h>

#include <array>
#include <numeric> // For iota.


namespace
{
template <typename TValue, unsigned int VLength>
void
Check_FixedArray_supports_retrieving_values_by_range_based_for_loop()
{
  std::array<TValue, VLength> stdArray;

  // Assign a different value (1, 2, 3, ...) to each element.
  std::iota(stdArray.begin(), stdArray.end(), 1);

  // Test retrieving the values from a const FixedArray:
  const itk::FixedArray<TValue, VLength> constFixedArray{ stdArray };

  auto stdArrayIterator = stdArray.cbegin();

  for (auto value : constFixedArray)
  {
    // Expect the same value as the corresponding std::array element.
    EXPECT_EQ(value, *stdArrayIterator);
    ++stdArrayIterator;
  }

  // Expect that all values are checked, "up to the end".
  EXPECT_EQ(stdArrayIterator, stdArray.cend());

  // Now test retrieving the values from a non-const FixedArray:
  itk::FixedArray<TValue, VLength> nonConstFixedArray{ stdArray };

  stdArrayIterator = stdArray.cbegin();

  for (auto value : nonConstFixedArray)
  {
    // Again, expect the same value as the corresponding std::array element.
    EXPECT_EQ(value, *stdArrayIterator);
    ++stdArrayIterator;
  }

  // Again, expect that all values are checked.
  EXPECT_EQ(stdArrayIterator, stdArray.cend());
}


template <typename TValue, unsigned int VLength>
void
Check_FixedArray_supports_modifying_elements_by_range_based_for_loop()
{
  itk::FixedArray<TValue, VLength> fixedArray{};

  TValue value{};

  // Assign the values 1, 2, 3, etc.
  for (auto & ref : fixedArray)
  {
    ++value;
    ref = value;
  }

  // Now check if the array has got the expected values.
  TValue expectedValue{};

  for (unsigned int i = 0; i < VLength; ++i)
  {
    ++expectedValue;
    EXPECT_EQ(fixedArray[i], expectedValue);
  }
}


#if !defined(ITK_LEGACY_REMOVE)
template <typename TValue, unsigned int VLength>
void
Check_new_reverse_iterator_behaves_like_old_ReverseIterator()
{
  using FixedArrayType = itk::FixedArray<TValue, VLength>;
  FixedArrayType fixedArray{};

  // Assign a different value (1, 2, 3, ...) to each element.
  std::iota(fixedArray.begin(), fixedArray.end(), 1);

  auto newIterator = fixedArray.rbegin();
  auto oldIterator = fixedArray.rBegin();

  const auto newEnd = fixedArray.rend();
  const auto oldEnd = fixedArray.rEnd();

  while ((newIterator != newEnd) && (oldIterator != oldEnd))
  {
    EXPECT_EQ(*newIterator, *oldIterator);
    ++newIterator;
    ++oldIterator;
  }
  EXPECT_EQ(newIterator, newEnd);
  EXPECT_EQ(oldIterator, oldEnd);
}
#endif

template <typename TValue, unsigned int VLength>
void
Check_const_and_non_const_reverse_iterators_retrieve_same_values()
{
  using FixedArrayType = itk::FixedArray<TValue, VLength>;
  using ConstIteratorType = typename FixedArrayType::const_reverse_iterator;
  using NonConstIteratorType = typename FixedArrayType::reverse_iterator;

  static_assert(!std::is_same<ConstIteratorType, NonConstIteratorType>::value,
                "Const and non-const reverse_iterator types must be different!");

  FixedArrayType fixedArray{};

  // Assign a different value (1, 2, 3, ...) to each element.
  std::iota(fixedArray.begin(), fixedArray.end(), 1);

  ConstIteratorType    constIterator = fixedArray.crbegin();
  NonConstIteratorType nonConstIterator = fixedArray.rbegin();

  const ConstIteratorType    constEnd = fixedArray.crend();
  const NonConstIteratorType nonConstEnd = fixedArray.rend();

  while ((constIterator != constEnd) && (nonConstIterator != nonConstEnd))
  {
    EXPECT_EQ(*constIterator, *nonConstIterator);
    ++constIterator;
    ++nonConstIterator;
  }
  EXPECT_EQ(constIterator, constEnd);
  EXPECT_EQ(nonConstIterator, nonConstEnd);
}


template <typename TValue, unsigned int VLength>
void
Check_reverse_iterators_allow_filling_a_FixedArray()
{
  using FixedArrayType = itk::FixedArray<TValue, VLength>;
  FixedArrayType fixedArray{};

  // Fill with ones, and then check the result.
  std::fill(fixedArray.rbegin(), fixedArray.rend(), 1);
  EXPECT_EQ(fixedArray, FixedArrayType::Filled(1));
}


template <typename TValue, unsigned int VLength>
void
Check_iterators_increment_return_value()
{
  using FixedArrayType = itk::FixedArray<TValue, VLength>;
  FixedArrayType fixedArray{};

  std::iota(fixedArray.begin(), fixedArray.end(), 1);

  typename FixedArrayType::iterator         newIterator = fixedArray.begin();
  typename FixedArrayType::Iterator         oldIterator = fixedArray.Begin();
  typename FixedArrayType::reverse_iterator newReverseIterator = fixedArray.rbegin();
#if !defined(ITK_LEGACY_REMOVE)
  typename FixedArrayType::ReverseIterator oldReverseIterator = fixedArray.rBegin();
#endif

  unsigned int index = 0;
  unsigned int reverseIndex = VLength - 1;
  for (unsigned int i = 0; i < VLength; ++i)
  {
    EXPECT_EQ(*newIterator++, fixedArray[index]);
    EXPECT_EQ(*newReverseIterator++, fixedArray[reverseIndex]);
    EXPECT_EQ(*oldIterator++, fixedArray[index]);
#if !defined(ITK_LEGACY_REMOVE)
    EXPECT_EQ(*oldReverseIterator++, fixedArray[reverseIndex]);
#endif
    index++;
    reverseIndex--;
  }

  newIterator = fixedArray.begin();
  oldIterator = fixedArray.Begin();
  newReverseIterator = fixedArray.rbegin();
#if !defined(ITK_LEGACY_REMOVE)
  oldReverseIterator = fixedArray.rBegin();
#endif

  index = 0;
  reverseIndex = VLength - 1;
  for (unsigned int i = 0; i < VLength - 1; ++i)
  {
    index++;
    reverseIndex--;
    EXPECT_EQ(*++newIterator, fixedArray[index]);
    EXPECT_EQ(*++newReverseIterator, fixedArray[reverseIndex]);
    EXPECT_EQ(*++oldIterator, fixedArray[index]);
#if !defined(ITK_LEGACY_REMOVE)
    EXPECT_EQ(*++oldReverseIterator, fixedArray[reverseIndex]);
#endif
  }
}

template <int VFillValue>
constexpr bool
Is_Filled_FixedArray_correctly_filled()
{
  using FixedArrayType = itk::FixedArray<int>;

  constexpr auto filledFixedArray = FixedArrayType::Filled(VFillValue);

  for (unsigned int i{}; i < FixedArrayType::Length; ++i)
  {
    if (filledFixedArray[i] != VFillValue)
    {
      return false;
    }
  }
  return true;
}


} // End of namespace

static_assert(Is_Filled_FixedArray_correctly_filled<0>() && Is_Filled_FixedArray_correctly_filled<1>() &&
                Is_Filled_FixedArray_correctly_filled<std::numeric_limits<int>::min()>() &&
                Is_Filled_FixedArray_correctly_filled<std::numeric_limits<int>::max()>(),
              "itk::FixedArray::Filled(value) should be correctly filled at compile-time");

static_assert(itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::FixedArray<int>>() &&
                itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::FixedArray<double, 1>>(),
              "Check constexpr begin() and end() of FixedArray.");


// Tests that the values of a FixedArray (either const or non-const) can be retrieved by a
// range-based for-loop.
TEST(FixedArray, SupportsRetrievingValuesByRangeBasedForLoop)
{
  Check_FixedArray_supports_retrieving_values_by_range_based_for_loop<double, 2>();
  Check_FixedArray_supports_retrieving_values_by_range_based_for_loop<int, 3>();
}


// Tests that FixedArray supports modifying its elements by a range-based for-loop.
TEST(FixedArray, SupportsModifyingElementsByRangeBasedForLoop)
{
  Check_FixedArray_supports_modifying_elements_by_range_based_for_loop<double, 2>();
  Check_FixedArray_supports_modifying_elements_by_range_based_for_loop<int, 3>();
}


#if !defined(ITK_LEGACY_REMOVE)
// Tests that the new reverse iterators (`rbegin()` and `rend()`, introduced with ITK 5.0)
// behave just like the old ones (`rBegin()` and `rEnd()`, originally from 2002).
TEST(FixedArray, NewReverseIteratorBehavesLikeOldReverseIterator)
{
  Check_new_reverse_iterator_behaves_like_old_ReverseIterator<double, 2>();
  Check_new_reverse_iterator_behaves_like_old_ReverseIterator<int, 3>();
}
#endif

// Tests that const and non-const reverse iterators retrieve exactly the same values.
TEST(FixedArray, ConstAndNonConstReverseIteratorRetrieveSameValues)
{
  Check_const_and_non_const_reverse_iterators_retrieve_same_values<double, 2>();
  Check_const_and_non_const_reverse_iterators_retrieve_same_values<int, 3>();
}


// Tests that reverse iterators can be used to fill a FixedArray.
TEST(FixedArray, CanBeFilledUsingReverseIterators)
{
  Check_reverse_iterators_allow_filling_a_FixedArray<double, 2>();
  Check_reverse_iterators_allow_filling_a_FixedArray<int, 3>();
}


// Tests that increment operators return a valid iterator
TEST(FixedArray, IteratorIncrementReturnValue)
{
  Check_iterators_increment_return_value<double, 2>();
  Check_iterators_increment_return_value<int, 3>();
}

// Tests data() and size() works
TEST(FixedArray, StdMemberFunctionsWork)
{
  using FixedArrayType = itk::FixedArray<double, 3>;

  // FixedArray::size() may be evaluated at compile-time, just like std::array::size().
  static_assert(FixedArrayType{}.size() == FixedArrayType::Dimension, "FixedArray::size() should equal its dimension");

  auto d3arr = FixedArrayType(3);
  d3arr[0] = 1;
  d3arr[1] = 2;
  d3arr[2] = 3;
  // size
  EXPECT_EQ(d3arr.size(), 3);
  // const and non-const data
  const auto cdata = d3arr.data();
  EXPECT_EQ(cdata[0], 1);
  d3arr.data()[0] = 10;
  EXPECT_EQ(cdata[0], 10);
}
