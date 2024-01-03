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
#include "itkCopy.h"
#include <gtest/gtest.h>

#include <array>
#include <cstdint> // For intmax_t.
#include <random>
#include <utility> // For pair.
#include <vector>


// Check compile-time evaluation:
static_assert(itk::Copy(0) == 0);
static_assert(itk::Copy(1) == 1);

namespace
{
// Returns a (const) reference to the specified argument.
template <typename T>
const T &
GetReference(const T & arg)
{
  return arg;
}
} // namespace


// Checks that an object returned by `itk::Copy(original)` compares equal to the original, even though it has a
// different address.
TEST(Copy, EqualToTheOriginalButHasDifferentAddress)
{
  const auto check = [](const auto & original) {
    auto && copy = itk::Copy(original);

    // The returned `copy` has an equal value...
    EXPECT_EQ(copy, original);

    // ... but a different address!
    EXPECT_NE(&copy, &original);
  };

  std::default_random_engine randomEngine{};
  const auto getRandomNumber = [&randomEngine] { return std::uniform_int_distribution<>{}(randomEngine); };

  check(getRandomNumber());
  check(std::vector<int>(3, getRandomNumber()));
  check(std::array<int, 3>{ getRandomNumber(), getRandomNumber(), getRandomNumber() });
}


// Tests that `itk::Copy` allows storing a copy of a value in a variable declared by `auto`, without a warning.
TEST(Copy, AllowsUsingAutoKeywordCopyingWithoutWarnings)
{
  std::vector<std::intmax_t> originalVector{ 1, 2, 3, 4 };

  // Without `itk::Copy`, the statement `const auto copyOfVector = GetReference(originalVector);` might trigger a
  // warning, saying something like "`auto` doesn't deduce references. A possibly unintended copy is being made" (MSVC
  // 2022 IntelliSense Code Linter).
  const auto copyOfVector = itk::Copy(GetReference(originalVector));

  // Clear the original (not the "copy").
  originalVector = {};

  // In this case, it is essential that the "copy" is not just a reference to the original, because the value of the
  // original object is modified after that copy took place. This modification should not affect the copy.
  ASSERT_NE(originalVector, copyOfVector);

  // A similar test for the equivalent `std::array`:
  std::array<std::intmax_t, 4> originalArray{ 1, 2, 3, 4 };

  // MSVC 2022 Warning C26820 ("This is a potentially expensive copy operation. Consider using a reference unless a copy
  // is required.") is not raised for types whose size isn't more than twice the platform-dependent pointer size,
  // according to https://learn.microsoft.com/en-us/cpp/code-quality/c26820?view=msvc-170
  static_assert(
    sizeof(originalArray) > 2 * sizeof(void *),
    "The array must be big enough to _possibly_ trigger compiler warnings about the expensiveness of copying!");

  const auto copyOfArray = itk::Copy(GetReference(originalArray));

  // Reset the elements of the original array (not the "copy").
  originalArray = {};

  // The original and the copy should be different now, because the copy is not reset.
  ASSERT_NE(originalArray, copyOfArray);

  // Sanity check: the copy of the vector and the copy of the array still have the same elements.
  EXPECT_TRUE(std::equal(copyOfVector.cbegin(), copyOfVector.cend(), copyOfArray.cbegin(), copyOfArray.cend()));

  // The following demonstates an `itk::Copy` use case when having structured bindings:
  auto originalPair =
    std::make_pair(std::vector<std::intmax_t>{ 1, 2, 3, 4 }, std::array<std::intmax_t, 4>{ 1, 2, 3, 4 });
  const auto [first, second] = itk::Copy(GetReference(originalPair));
  const auto copyOfPair = itk::Copy(GetReference(originalPair));
  originalPair = {};
  ASSERT_NE(originalPair, copyOfPair);

  EXPECT_EQ(copyOfPair, std::make_pair(first, second));
}
