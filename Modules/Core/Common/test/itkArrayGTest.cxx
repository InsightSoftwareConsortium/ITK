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
#include "itkArray.h"
#include <gtest/gtest.h>
#include <type_traits> // For is_nothrow_move_constructible_v and is_nothrow_move_assignable_v.


static_assert(std::is_nothrow_move_constructible_v<itk::Array<int>> &&
                std::is_nothrow_move_constructible_v<itk::Array<double>>,
              "Array should have a `noexcept` move-constructor!");
static_assert(std::is_nothrow_move_assignable_v<itk::Array<int>> &&
                std::is_nothrow_move_assignable_v<itk::Array<double>>,
              "Array should have a `noexcept` move-assignment operator!");


// Tests that when move-constructing an Array, the data is "taken" from the original, and "moved" to the newly
// constructed object.
TEST(Array, MoveConstruct)
{
  const auto checkMoveConstruct = [](auto && original) {
    const auto * const originalData{ original.data_block() };
    const std::size_t  originalSize{ original.size() };

    const auto moveConstructed = std::move(original);

    // After the "move", the move-constructed object has retrieved the original data.
    EXPECT_EQ(moveConstructed.data_block(), originalData);
    EXPECT_EQ(moveConstructed.size(), originalSize);

    // After the "move", the original is left empty.
    EXPECT_EQ(original.data_block(), nullptr);
    EXPECT_EQ(original.size(), 0U);
  };

  checkMoveConstruct(itk::Array<int>());
  checkMoveConstruct(itk::Array<int>(1U));
  checkMoveConstruct(itk::Array<double>(2U));
}


// Tests that when move-assigning an Array, the data is "taken" from the original, and "moved" to the target of the
// move-assignment.
TEST(Array, MoveAssign)
{
  const auto checkMoveAssign = [](auto original) {
    const auto * const originalData{ original.data_block() };
    const std::size_t  originalSize{ original.size() };

    decltype(original) moveAssigmentTarget;
    moveAssigmentTarget = std::move(original);

    // After the "move", the target of the move-assignment has retrieved the original data.
    EXPECT_EQ(moveAssigmentTarget.data_block(), originalData);
    EXPECT_EQ(moveAssigmentTarget.size(), originalSize);

    // After the "move", the original is left empty.
    EXPECT_EQ(original.data_block(), nullptr);
    EXPECT_EQ(original.size(), 0U);
  };

  checkMoveAssign(itk::Array<int>());
  checkMoveAssign(itk::Array<int>(1U));
  checkMoveAssign(itk::Array<double>(2U));
}
