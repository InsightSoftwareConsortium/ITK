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
#include "itkSize.h"
#include "itkRangeGTestUtilities.h"
#include <gtest/gtest.h>
#include <limits>
#include <type_traits> // For integral_constant.


namespace
{
template <unsigned int VDimension>
constexpr std::integral_constant<unsigned int, VDimension> DimensionConstant{};


template <unsigned int VDimension>
void
Expect_Filled_returns_Size_with_specified_value_for_each_element()
{
  using itk::SizeValueType;
  using SizeType = itk::Size<VDimension>;

  // Tests that all elements from SizeType::Filled(sizeValue) get the specified value.
  const auto Expect_Filled_with_value = [](const SizeValueType sizeValue) {
    for (const auto filledSizeValue : SizeType::Filled(sizeValue))
    {
      EXPECT_EQ(filledSizeValue, sizeValue);
    }
  };

  // Test for sizeValue 0, 1, 2, and its maximum.
  for (SizeValueType sizeValue = 0; sizeValue < 3; ++sizeValue)
  {
    Expect_Filled_with_value(sizeValue);
  }
  Expect_Filled_with_value(std::numeric_limits<SizeValueType>::max());
}


template <itk::SizeValueType VFillValue>
constexpr bool
Is_Filled_Size_correctly_filled()
{
  for (const auto actualValue : itk::Size<>::Filled(VFillValue).m_InternalArray)
  {
    if (actualValue != VFillValue)
    {
      return false;
    }
  }
  return true;
}


} // namespace


static_assert(Is_Filled_Size_correctly_filled<0>() && Is_Filled_Size_correctly_filled<1>() &&
                Is_Filled_Size_correctly_filled<std::numeric_limits<itk::IndexValueType>::max()>(),
              "itk::Size::Filled(value) should be correctly filled at compile-time");

static_assert(itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::Size<>>() &&
                itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::Size<1>>(),
              "Check constexpr begin() and end() of Size.");


// Tests that itk::Size::Filled(value) returns an itk::Size with the
// specified value for each element.
TEST(Size, FilledReturnsSizeWithSpecifiedValueForEachDimension)
{
  // Test for 1-D, 2-D, and 3-D.
  Expect_Filled_returns_Size_with_specified_value_for_each_element<1>();
  Expect_Filled_returns_Size_with_specified_value_for_each_element<2>();
  Expect_Filled_returns_Size_with_specified_value_for_each_element<3>();
}


TEST(Size, Make)
{
  static_assert((decltype(itk::MakeSize(1, 1))::Dimension == 2) && (decltype(itk::MakeSize(1, 1, 1))::Dimension == 3),
                "The dimension of the created itk::Size should equal the number of arguments");

  EXPECT_EQ(itk::MakeSize(0, 0), itk::Size<2>());
  EXPECT_EQ(itk::MakeSize(0, 0, 0), itk::Size<3>());

  const auto itkSize = itk::MakeSize(1, 2, 3, 4);
  const auto values = { 1, 2, 3, 4 };
  EXPECT_TRUE(std::equal(itkSize.begin(), itkSize.end(), values.begin(), values.end()));
}


// Tests that `Size<dimension>::Fill(sizeValue)` corresponds with `Size<dimension>::Filled(sizeValue)`.
TEST(Size, Fill)
{
  const auto check = [](const auto dimensionConstant) {
    for (const auto sizeValue : { itk::SizeValueType{ 0 },
                                  itk::SizeValueType{ 1 },
                                  itk::SizeValueType{ 2 },
                                  std::numeric_limits<itk::SizeValueType>::max() })
    {
      itk::Size<dimensionConstant> actualSize;
      actualSize.Fill(sizeValue);
      const auto expectedSize = itk::Size<dimensionConstant>::Filled(sizeValue);
      EXPECT_EQ(actualSize, expectedSize);
    }
  };

  // Check Size::Fill for 1D, 2D, and 3D:
  check(DimensionConstant<1>);
  check(DimensionConstant<2>);
  check(DimensionConstant<3>);
}
