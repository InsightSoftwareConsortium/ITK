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
#include "itkArray2D.h"
#include <gtest/gtest.h>
#include <limits>


// Tests that Array2D may be constructed with an initial value for each element.
TEST(Array2D, ConstructorSupportsInitialValue)
{
  const auto checkConstructor =
    [](const unsigned int numberOfRows, const unsigned int numberOfCols, const auto initialValue) {
      using ValueType = std::remove_const_t<decltype(initialValue)>;

      const itk::Array2D<ValueType> array2D(numberOfRows, numberOfCols, initialValue);

      EXPECT_EQ(array2D.rows(), numberOfRows);
      EXPECT_EQ(array2D.columns(), numberOfCols);

      for (const auto & element : array2D)
      {
        EXPECT_EQ(element, initialValue);
      }
    };

  for (const auto initialValue : { -1, 0, 1 })
  {
    checkConstructor(0, 0, initialValue);
    checkConstructor(1, 2, initialValue);
  }
  for (const auto initialValue : { std::numeric_limits<double>::min(), std::numeric_limits<double>::max() })
  {
    checkConstructor(0, 0, initialValue);
    checkConstructor(1, 2, initialValue);
  }
}
