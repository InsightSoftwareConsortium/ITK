// SPDX-FileCopyrightText: Copyright NumFOCUS
// SPDX-License-Identifier: Apache-2.0
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
#include "itkVectorImage.h"
#include "itkImageRegionIterator.h"
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


// Regression test: ensure that setting a VectorImage pixel from a
// VariableLengthVector that is shorter than the image's
// NumberOfComponentsPerPixel does not crash. Before commit
// 1d87efa529, `VariableLengthVector(length)` with length=0 allocated a
// valid (zero-size) heap block whose address was non-null. That commit
// changed the ctor to store `nullptr` for length=0 and caused
// DefaultVectorPixelAccessor::Set — which copies m_VectorLength
// elements from the source VLV regardless of the source's own size —
// to dereference the source's null data pointer, producing SIGSEGV.
// This test documents that a length-mismatched source VLV is safe to
// pass; it is permitted for the target pixel to receive
// implementation-defined values, but the call must not crash.
TEST(VariableLengthVector, VectorImageSetPixelFromShorterSourceIsSafe)
{
  using ValueType = float;
  using ImageType = itk::VectorImage<ValueType, 3>;
  using PixelType = ImageType::PixelType;

  constexpr unsigned int      componentsPerPixel = 3;
  auto                        image = ImageType::New();
  const ImageType::SizeType   size = { { 2, 2, 2 } };
  const ImageType::RegionType region{ size };
  image->SetRegions(region);
  image->SetNumberOfComponentsPerPixel(componentsPerPixel);
  image->Allocate();

  // Seed every pixel with a known sentinel value so we can assert the
  // fix's "trailing components left unchanged" guarantee.
  constexpr ValueType sentinelValue = 42.0f;
  PixelType           sentinel(componentsPerPixel);
  sentinel.Fill(sentinelValue);
  for (itk::ImageRegionIterator<ImageType> seedIt(image, region); !seedIt.IsAtEnd(); ++seedIt)
  {
    seedIt.Set(sentinel);
  }

  // Pre-regression, VLV(0) had m_Data = new ValueType[0] (non-null).
  // Post-regression, m_Data is nullptr. The iterator-based Set()
  // path must tolerate that without dereferencing the null buffer.
  PixelType shorter(0);
  EXPECT_EQ(shorter.GetSize(), 0u);

  itk::ImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  ASSERT_FALSE(it.IsAtEnd());
  // The regression manifests here: before the fix, this produces
  // a SIGSEGV due to memcpy/load from a null source buffer.
  ASSERT_NO_FATAL_FAILURE(it.Set(shorter));

  // Verify the fix's guarantee: a length-0 source is a no-op; the
  // destination pixel retains its prior (sentinel) value.
  const auto unchanged = it.Get();
  ASSERT_EQ(unchanged.GetSize(), componentsPerPixel);
  EXPECT_FLOAT_EQ(unchanged[0], sentinelValue);
  EXPECT_FLOAT_EQ(unchanged[1], sentinelValue);
  EXPECT_FLOAT_EQ(unchanged[2], sentinelValue);
}


// Companion: verify the normal, length-matched path still works.
TEST(VariableLengthVector, VectorImageSetPixelFromMatchedSourceWorks)
{
  using ValueType = float;
  using ImageType = itk::VectorImage<ValueType, 3>;
  using PixelType = ImageType::PixelType;

  constexpr unsigned int      componentsPerPixel = 3;
  auto                        image = ImageType::New();
  const ImageType::SizeType   size = { { 2, 2, 2 } };
  const ImageType::RegionType region{ size };
  image->SetRegions(region);
  image->SetNumberOfComponentsPerPixel(componentsPerPixel);
  image->Allocate();

  PixelType matched(componentsPerPixel);
  matched[0] = 1.0f;
  matched[1] = 2.0f;
  matched[2] = 3.0f;

  itk::ImageRegionIterator<ImageType> it(image, region);
  it.GoToBegin();
  it.Set(matched);

  const auto roundTrip = it.Get();
  ASSERT_EQ(roundTrip.GetSize(), componentsPerPixel);
  EXPECT_FLOAT_EQ(roundTrip[0], 1.0f);
  EXPECT_FLOAT_EQ(roundTrip[1], 2.0f);
  EXPECT_FLOAT_EQ(roundTrip[2], 3.0f);
}
