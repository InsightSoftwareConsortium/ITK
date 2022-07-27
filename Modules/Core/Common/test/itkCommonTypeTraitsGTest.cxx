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
#include "itkFixedArray.h"
// Derived from FixedArray (1st generation)
#include "itkCovariantVector.h"
#include "itkRGBAPixel.h"
#include "itkRGBPixel.h"
#include "itkPoint.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkVector.h"
// 2nd generation Fixed Array
#include "itkContinuousIndex.h" // From Point

#include <gtest/gtest.h>
#include <type_traits>

// info: is_pod (c++20 deprecated) is equivalent to is_trivial && is_standard_layout
// info: type_traits cannot differentiate if an explicit move constructible/assignment exist.

TEST(CommonTypeTraits, FixedArrayIsPOD)
{
  using T = itk::FixedArray<float, 3>;
  EXPECT_TRUE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}
/************ First Generation FixedArray *************/
TEST(CommonTypeTraits, VectorIsPOD)
{
  using T = itk::Vector<float, 3>;
  EXPECT_TRUE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}

TEST(CommonTypeTraits, CovariantVectorIsPOD)
{
  using T = itk::CovariantVector<float, 3>;
  EXPECT_TRUE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}

TEST(CommonTypeTraits, PointIsPOD)
{
  using T = itk::Point<float>;
  EXPECT_TRUE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}

TEST(CommonTypeTraits, RGBAPixelIsNotPOD)
{
  using T = itk::RGBAPixel<unsigned int>;
  // Because initialized to zero
  EXPECT_FALSE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}

TEST(CommonTypeTraits, RGBPixelIsNotPOD)
{
  using T = itk::RGBPixel<unsigned int>;
  // Because initialized to zero
  EXPECT_FALSE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}

TEST(CommonTypeTraits, SymmetricSecondRankTensorIsNotPOD)
{
  using T = itk::SymmetricSecondRankTensor<float, 3>;
  // Because initialized to zero
  EXPECT_FALSE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}

/************ Second Generation FixedArray *************/
/* Derived from Point */
TEST(CommonTypeTraits, ContinuousIndexIsPOD)
{
  using T = itk::ContinuousIndex<float, 2>;
  EXPECT_TRUE(std::is_trivial<T>::value);
  EXPECT_TRUE(std::is_standard_layout<T>::value);
}

/************ FixedArray: noexcept move checks *************/

/* Dummy class without noexcept move constructors. */
struct NotNoexceptMove
{
  NotNoexceptMove() = default;
  NotNoexceptMove(NotNoexceptMove &&) {}
  NotNoexceptMove(const NotNoexceptMove &) = default;
};

/* Check that move-constructing a FixedArray works as move-constructing an aggregate.
 * Move constructors are noexcept when the contained class has a noexcept move constructor.
 * And when the contained class might throw at move, the qualifier automatically
 * propagates it to FixedArray.
 * This is the same behaviour as std::array. It proves there is no need to
 * add noexcept to the container. This would disallow using it
 * with might-throw-objects.
 */
TEST(CommonTypeTraits, FixedArrayIsNoExceptMovable)
{
  using IntArrayType = itk::FixedArray<int, 3>;
  EXPECT_TRUE(std::is_nothrow_move_constructible<IntArrayType>::value);
  EXPECT_TRUE(std::is_copy_constructible<IntArrayType>::value);
  using NotNoexceptMoveArrayType = itk::FixedArray<NotNoexceptMove, 3>;
  EXPECT_FALSE(std::is_nothrow_move_constructible<NotNoexceptMoveArrayType>::value);
  EXPECT_TRUE(std::is_copy_constructible<NotNoexceptMoveArrayType>::value);
}
