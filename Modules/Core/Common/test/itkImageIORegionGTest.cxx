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
#include "itkImageIORegion.h"
#include <gtest/gtest.h>
#include <random>
#include <type_traits> // For is_default_constructible, is_copy_constructible, etc.
#include <utility>     // For move.

namespace
{

template <typename TTypeTrait>
constexpr bool
StaticAssertValueOfTypeTrait()
{
  static_assert(TTypeTrait::value, "The value of this type trait should be true!");
  return TTypeTrait::value;
}


template <typename T>
constexpr bool
IsDefaultConstructibleCopyableNoThrowMovableAndDestructible()
{
  return StaticAssertValueOfTypeTrait<std::is_default_constructible<T>>() &&
         StaticAssertValueOfTypeTrait<std::is_copy_constructible<T>>() &&
         StaticAssertValueOfTypeTrait<std::is_copy_assignable<T>>() &&
         StaticAssertValueOfTypeTrait<std::is_nothrow_move_constructible<T>>() &&
         StaticAssertValueOfTypeTrait<std::is_nothrow_move_assignable<T>>() &&
         StaticAssertValueOfTypeTrait<std::is_nothrow_move_assignable<T>>();
}

static_assert(IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::ImageIORegion>(),
              "itk::ImageIORegion should be default-constructible, copyable, noexcept movable, and destructible.");


template <typename T>
void
Expect_CopyAndMoveConstructible(const T & value)
{
  const auto copiedValue = value;
  EXPECT_EQ(copiedValue, value);

  auto       nonConstValue = value;
  const auto movedValue = std::move(nonConstValue);
  EXPECT_EQ(movedValue, value);
}


template <typename T>
void
Expect_CopyAssignable(const T & initialValue, const T & newValue)
{
  T lhs(initialValue);
  lhs = newValue;
  EXPECT_EQ(lhs, newValue);
}


template <typename T>
void
Expect_CopyAssignableToSelf(const T & value)
{
  T self(value);

  // Slight contortion to avoid self-assignment warning with 'self = self'.
  const T & referenceToSelf = self;
  self = referenceToSelf;

  EXPECT_EQ(self, value);
}


template <typename T>
void
Expect_CopyAssignableToSameValue(const T & value)
{
  T self(value);
  self = value;
  EXPECT_EQ(self, value);
}


template <typename T>
void
Expect_MoveAssignableToSameValue(const T & value)
{
  T self(value);

  // Note that T(value) is an rvalue, that can be moved!
  self = T(value);
  EXPECT_EQ(self, value);
}


template <typename T>
void
Expect_MoveAssignable(const T & initialValue, const T & newValue)
{
  T lhs(initialValue);

  // Note that T(newValue) is an rvalue, that can be moved!
  lhs = T(newValue);
  EXPECT_EQ(lhs, newValue);
}


template <typename T>
void
Expect_Assignable(const T & value1, const T & value2)
{
  Expect_MoveAssignable(value1, value2);
  Expect_MoveAssignable(value2, value1);
  Expect_MoveAssignableToSameValue(value1);
  Expect_MoveAssignableToSameValue(value2);

  Expect_CopyAssignable(value1, value2);
  Expect_CopyAssignable(value2, value1);
  Expect_CopyAssignableToSameValue(value1);
  Expect_CopyAssignableToSameValue(value2);
  Expect_CopyAssignableToSelf(value1);
  Expect_CopyAssignableToSelf(value2);
}


void
SetRandomIndexAndSize(itk::ImageIORegion & region)
{
  std::mt19937                                       randomNumberEngine;
  std::uniform_int_distribution<itk::IndexValueType> indexDistribution(std::numeric_limits<itk::IndexValueType>::min());
  std::uniform_int_distribution<itk::SizeValueType>  sizeDistribution(std::numeric_limits<itk::SizeValueType>::min());

  const unsigned imageDimension{ region.GetImageDimension() };

  for (unsigned i{}; i < imageDimension; ++i)
  {
    region.SetIndex(i, indexDistribution(randomNumberEngine));
    region.SetSize(i, sizeDistribution(randomNumberEngine));
  }
}


itk::ImageIORegion
GenerateRandomRegion(const unsigned imageDimension)
{
  itk::ImageIORegion region(imageDimension);
  SetRandomIndexAndSize(region);
  return region;
}

} // namespace


TEST(ImageIORegion, IsTwoDimensionalByDefault)
{
  const itk::ImageIORegion expectedTwoDimensionalRegion(2);

  EXPECT_EQ(itk::ImageIORegion(), expectedTwoDimensionalRegion);

  itk::ImageIORegion defaultInitializedRegion;
  EXPECT_EQ(defaultInitializedRegion, expectedTwoDimensionalRegion);
}


TEST(ImageIORegion, IsCopyAndMoveConstructible)
{
  Expect_CopyAndMoveConstructible(itk::ImageIORegion());

  Expect_CopyAndMoveConstructible(itk::ImageIORegion(3));

  itk::ImageIORegion region(3);
  SetRandomIndexAndSize(region);

  Expect_CopyAndMoveConstructible(region);
}


TEST(ImageIORegion, IsAssignable)
{
  Expect_Assignable(GenerateRandomRegion(2), GenerateRandomRegion(2));
  Expect_Assignable(GenerateRandomRegion(2), GenerateRandomRegion(3));
}
