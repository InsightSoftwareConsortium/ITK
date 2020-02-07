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
#include "itkPoint.h"
#include <gtest/gtest.h>

#include <iterator> // For begin and end.
#include <numeric>  // For iota.

namespace
{
template <unsigned VDimension>
void
Expect_Point_can_be_constructed_by_std_array()
{
  using ValueType = double;
  using PointType = itk::Point<ValueType, VDimension>;

  std::array<ValueType, VDimension> stdArray;

  // Assign a different value (0, 1, 2, ...) to each element.
  std::iota(std::begin(stdArray), std::end(stdArray), 0.0);

  // Now construct the point (using the explicit Point constructor for std::array).
  const PointType point(stdArray);

  // Check that the values of all element are copied.
  EXPECT_TRUE(std::equal(point.cbegin(), point.cend(), stdArray.cbegin()));
}
} // namespace


// Tests that a Point can be constructed by specifying
// its coordinates by an std::array<ValueType, VDimension>.
TEST(Point, CanBeConstructedByStdArray)
{
  // Test for 2-D and 3-D.
  Expect_Point_can_be_constructed_by_std_array<2>();
  Expect_Point_can_be_constructed_by_std_array<3>();
}
