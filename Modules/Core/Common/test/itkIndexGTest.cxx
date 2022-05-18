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

// First include the header file to be tested:
#include "itkIndex.h"
#include "itkRangeGTestUtilities.h"
#include <gtest/gtest.h>
#include <initializer_list>
#include <limits>

namespace
{
template <unsigned int VDimension>
void
Expect_Filled_returns_Index_with_specified_value_for_each_element()
{
  using itk::IndexValueType;
  using IndexType = itk::Index<VDimension>;

  // Tests that all elements from IndexType::Filled(indexValue) get the specified value.
  const auto Expect_Filled_with_value = [](const IndexValueType indexValue) {
    for (const auto filledIndexValue : IndexType::Filled(indexValue))
    {
      EXPECT_EQ(filledIndexValue, indexValue);
    }
  };

  // Test for indexValue 0, 1, 2, and its maximum.
  for (IndexValueType indexValue = 0; indexValue <= 2; ++indexValue)
  {
    Expect_Filled_with_value(indexValue);
  }
  Expect_Filled_with_value(std::numeric_limits<IndexValueType>::max());
}


template <itk::IndexValueType VFillValue>
constexpr bool
Is_Filled_Index_correctly_filled()
{
  for (const auto actualValue : itk::Index<>::Filled(VFillValue).m_InternalArray)
  {
    if (actualValue != VFillValue)
    {
      return false;
    }
  }
  return true;
}

} // namespace


static_assert(Is_Filled_Index_correctly_filled<0>() && Is_Filled_Index_correctly_filled<1>() &&
                Is_Filled_Index_correctly_filled<std::numeric_limits<itk::IndexValueType>::min()>() &&
                Is_Filled_Index_correctly_filled<std::numeric_limits<itk::IndexValueType>::max()>(),
              "itk::Index::Filled(value) should be correctly filled at compile-time");

static_assert(itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::Index<>>() &&
                itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::Index<1>>(),
              "Check constexpr begin() and end() of Index.");


// Tests that itk::Index::Filled(value) returns an itk::Index with the
// specified value for each element.
TEST(Index, FilledReturnsIndexWithSpecifiedValueForEachElement)
{
  // Test for 1-D and 3-D.
  Expect_Filled_returns_Index_with_specified_value_for_each_element<1>();
  Expect_Filled_returns_Index_with_specified_value_for_each_element<3>();
}


TEST(Index, Make)
{
  static_assert((decltype(itk::MakeIndex(1, 1))::Dimension == 2) && (decltype(itk::MakeIndex(1, 1, 1))::Dimension == 3),
                "The dimension of the created itk::Size should equal the number of arguments");

  EXPECT_EQ(itk::MakeIndex(0, 0), itk::Index<2>());
  EXPECT_EQ(itk::MakeIndex(0, 0, 0), itk::Index<3>());

  const auto itkIndex = itk::MakeIndex(1, 2, 3, 4);
  const auto values = { 1, 2, 3, 4 };
  EXPECT_TRUE(std::equal(itkIndex.begin(), itkIndex.end(), values.begin(), values.end()));
}
