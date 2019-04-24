/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkFixedArray.h"
#include <gtest/gtest.h>

#include <array>
#include <numeric>  // For iota.


namespace
{
  template< typename TValue, unsigned int VLength >
  void Check_FixedArray_supports_retrieving_values_by_range_based_for_loop()
  {
    std::array<TValue, VLength> stdArray{};

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


  template< typename TValue, unsigned int VLength >
  void Check_FixedArray_supports_modifying_elements_by_range_based_for_loop()
  {
    itk::FixedArray<TValue, VLength> fixedArray{};

    TValue value{};

    // Assign the values 1, 2, 3, etc.
    for (auto& ref : fixedArray)
    {
      ++value;
      ref = value;
    }

    // Now check if the array has got the expected values.
    TValue expectedValue{};

    for (std::size_t i = 0; i < VLength; ++i)
    {
      ++expectedValue;
      EXPECT_EQ(fixedArray[i], expectedValue);
    }
  }

} // End of namespace


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
