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
#include "itkVariableLengthVector.h"
#include "itkRangeGTestUtilities.h"
#include <gtest/gtest.h>
#include <numeric> // For iota.
#include <random>  // For mt19937.

using itk::RangeGTestUtilities;


static_assert(RangeGTestUtilities::CheckContainerRequirementsOnNestedTypes<itk::VariableLengthVector<int>>() &&
              RangeGTestUtilities::CheckContainerRequirementsOnNestedTypes<itk::VariableLengthVector<double>>());


// Tests that begin() == end() for a default-constructed object.
TEST(VariableLengthVector, BeginIsEndWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<itk::VariableLengthVector<int>>();
  RangeGTestUtilities::ExpectBeginIsEndWhenRangeIsDefaultConstructed<itk::VariableLengthVector<double>>();

  RangeGTestUtilities::ExpectReverseBeginIsReverseEndWhenRangeIsDefaultConstructed<itk::VariableLengthVector<int>>();
  RangeGTestUtilities::ExpectReverseBeginIsReverseEndWhenRangeIsDefaultConstructed<itk::VariableLengthVector<double>>();
}


// Tests that size() returns 0 for a default-constructed object.
TEST(VariableLengthVector, SizeIsZeroWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<itk::VariableLengthVector<int>>();
  RangeGTestUtilities::ExpectZeroSizeWhenRangeIsDefaultConstructed<itk::VariableLengthVector<double>>();
}


// Tests empty() for a default-constructed object.
TEST(VariableLengthVector, IsEmptyWhenDefaultConstructed)
{
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<itk::VariableLengthVector<int>>();
  RangeGTestUtilities::ExpectRangeIsEmptyWhenDefaultConstructed<itk::VariableLengthVector<double>>();
}


// Tests that the distance from begin() to end() equals its size().
TEST(VariableLengthVector, DistanceFromBeginToEndEqualsSize)
{
  for (const unsigned int size : { 0, 1, 2 })
  {
    RangeGTestUtilities::ExpectDistanceFromBeginToEndEqualsSize(itk::VariableLengthVector<int>(size));
    RangeGTestUtilities::ExpectDistanceFromBeginToEndEqualsSize(itk::VariableLengthVector<double>(size));
  }
}


// Tests that `distance(&front, &back) + 1` is equal to `size`, for a non-empty VariableLengthVector.
TEST(VariableLengthVector, DistanceFromFrontToBackPlusOneEqualsSize)
{
  for (const unsigned int size : { 1, 2 })
  {
    RangeGTestUtilities::ExpectDistanceFromFrontToBackPlusOneEqualsSize(itk::VariableLengthVector<int>(size));
    RangeGTestUtilities::ExpectDistanceFromFrontToBackPlusOneEqualsSize(itk::VariableLengthVector<double>(size));
  }
}


// Tests that `at(i)` is equal to `[i]`, for a non-empty VariableLengthVector.
TEST(VariableLengthVector, AtReturnsSameElementAsSubscriptOperator)
{
  std::mt19937                    randomNumberEngine{};
  std::uniform_int_distribution<> randomNumberDistribution{};

  for (const unsigned int size : { 1, 2, 3 })
  {
    itk::VariableLengthVector<int> variableLengthVector(size);

    std::generate(
      variableLengthVector.begin(), variableLengthVector.end(), [&randomNumberEngine, &randomNumberDistribution] {
        return randomNumberDistribution(randomNumberEngine);
      });

    for (unsigned int i = 0; i < size; ++i)
    {
      EXPECT_EQ(variableLengthVector.at(i), variableLengthVector[i]);
      EXPECT_EQ(&(variableLengthVector.at(i)), &(variableLengthVector[i]));
    }
  }
}


// Tests that `at(i)` throws an `std::out_of_range` exception when `i >= size`.
TEST(VariableLengthVector, AtThrowsOutOfRange)
{
  for (const unsigned int size : { 0, 1, 2 })
  {
    itk::VariableLengthVector<int> variableLengthVector(size);
    std::fill(variableLengthVector.begin(), variableLengthVector.end(), 0);

    for (const unsigned int i : { size, UINT_MAX })
    {
      EXPECT_THROW(static_cast<void>(variableLengthVector.at(i)), std::out_of_range);
    }
  }
}

// Tests reverse iteration.
TEST(VariableLengthVector, ReverseIteration)
{
  for (const unsigned int size : { 0, 1, 2, 3 })
  {
    itk::VariableLengthVector<int> variableLengthVector(size);

    // Fill with { 1, 2, 3, ..., size }.
    std::iota(variableLengthVector.begin(), variableLengthVector.end(), 1);

    // Check that reverse iteration yields { size, size-1, ..., 2, 1 }.
    auto expectedValue = static_cast<int>(size);

    const auto reverseEnd = variableLengthVector.crend();

    for (auto reverseIterator = variableLengthVector.crbegin(); reverseIterator != reverseEnd; ++reverseIterator)
    {
      EXPECT_EQ(*reverseIterator, expectedValue);
      --expectedValue;
    }
  }
}


// Tests constructing with the specified length and value.
TEST(VariableLengthVector, ConstructWithSpecifiedLengthAndValue)
{
  using ValueType = int;
  using VariableLengthVectorType = itk::VariableLengthVector<ValueType>;
  using ValueLimits = std::numeric_limits<ValueType>;

  for (const auto value : { ValueLimits::min(), ValueType{}, ValueType{ 1 }, ValueLimits::max() })
  {
    EXPECT_TRUE(VariableLengthVectorType(0, value).empty());

    for (unsigned int length{ 1 }; length < 4; ++length)
    {
      const VariableLengthVectorType variableLengthVector(length, value);

      EXPECT_EQ(variableLengthVector.size(), length);
      EXPECT_EQ(std::count(variableLengthVector.cbegin(), variableLengthVector.cend(), value), length);
    }
  }
}
