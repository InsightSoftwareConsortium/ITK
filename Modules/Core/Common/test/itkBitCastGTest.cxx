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
#include "itkBitCast.h"
#include <gtest/gtest.h>
#include <cstring> // For memcmp.

namespace
{
template <typename TDestination, class TSource>
void
Expect_return_value_is_bitwise_equal_to_function_argument(const TSource & source)
{
  const auto result = itk::bit_cast<TDestination>(source);
  EXPECT_EQ(std::memcmp(&result, &source, sizeof(TSource)), 0);
}
} // namespace


TEST(BitCast, ResultIsBitwiseEqualToArgument)
{
  for (const int i : { -1, 0, 1 })
  {
    Expect_return_value_is_bitwise_equal_to_function_argument<unsigned int>(i);
  }

  int value;
  (void)value;
  Expect_return_value_is_bitwise_equal_to_function_argument<intptr_t>(&value);
}
