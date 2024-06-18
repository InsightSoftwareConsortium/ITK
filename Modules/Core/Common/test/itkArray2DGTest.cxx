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
#include <type_traits> // For is_nothrow_move_constructible_v and is_nothrow_move_assignable_v.


static_assert(std::is_nothrow_move_constructible_v<itk::Array2D<int>> &&
                std::is_nothrow_move_constructible_v<itk::Array2D<double>>,
              "Array2D should have a `noexcept` move-constructor!");
static_assert(std::is_nothrow_move_assignable_v<itk::Array2D<int>> &&
                std::is_nothrow_move_assignable_v<itk::Array2D<double>>,
              "Array2D should have a `noexcept` move-assignment operator!");

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


// Tests that when move-constructing an Array2D, the data is "taken" from the original, and "moved" to the newly
// constructed object.
TEST(Array2D, MoveConstruct)
{
  const auto checkMoveConstruct = [](auto && original) {
    const auto * const * const originalDataArray{ original.data_array() };
    const unsigned int         originalSize{ original.size() };

    const auto moveConstructed = std::move(original);

    // After the "move", the move-constructed object has retrieved the original data.
    EXPECT_EQ(moveConstructed.data_array(), originalDataArray);
    EXPECT_EQ(moveConstructed.size(), originalSize);

    // After the "move", the original is left empty.
    EXPECT_EQ(original.data_array(), nullptr);
    EXPECT_EQ(original.size(), 0U);
  };

  checkMoveConstruct(itk::Array2D<int>());
  checkMoveConstruct(itk::Array2D<int>(1U, 1U));
  checkMoveConstruct(itk::Array2D<double>(1U, 2U));
}


// Tests that when move-assigning an Array2D, the data is "taken" from the original, and "moved" to the target of the
// move-assignment.
TEST(Array2D, MoveAssign)
{
  const auto checkMoveAssign = [](auto original) {
    const auto * const * const originalDataArray{ original.data_array() };
    const unsigned int         originalSize{ original.size() };

    decltype(original) moveAssigmentTarget;
    moveAssigmentTarget = std::move(original);

    // After the "move", the target of the move-assignment has retrieved the original data.
    EXPECT_EQ(moveAssigmentTarget.data_array(), originalDataArray);
    EXPECT_EQ(moveAssigmentTarget.size(), originalSize);

    // After the "move", the original is left empty.
    EXPECT_EQ(original.data_array(), nullptr);
    EXPECT_EQ(original.size(), 0U);
  };

  checkMoveAssign(itk::Array2D<int>());
  checkMoveAssign(itk::Array2D<int>(1U, 1U));
  checkMoveAssign(itk::Array2D<double>(1U, 2U));
}
