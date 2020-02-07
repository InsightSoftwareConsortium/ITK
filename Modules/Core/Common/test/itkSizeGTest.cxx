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
#include "itkSize.h"
#include <gtest/gtest.h>
#include <limits>

namespace
{
template <unsigned VDimension>
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
} // namespace


// Tests that itk::Size::Filled(value) returns an itk::Size with the
// specified value for each element.
TEST(Size, FilledReturnsSizeWithSpecifiedValueForEachDimension)
{
  // Test for 1-D, 2-D, and 3-D.
  Expect_Filled_returns_Size_with_specified_value_for_each_element<1>();
  Expect_Filled_returns_Size_with_specified_value_for_each_element<2>();
  Expect_Filled_returns_Size_with_specified_value_for_each_element<3>();
}
