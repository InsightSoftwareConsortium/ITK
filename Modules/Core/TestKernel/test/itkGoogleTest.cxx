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

#include "itkGTest.h"
#include "itkMath.h"
#include "itkOptimizerParameters.h"

// Minimal test to verify Google Test works
TEST(GoogleTest, t1)
{
  void * ptr = nullptr;
  ASSERT_TRUE((ptr == nullptr));
  EXPECT_TRUE((ptr == nullptr));
}

// cover ITK Google testing utilities
TEST(GoogleTest, TypedefsAndConstructors_Dimension2)
{

  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  PointType pt1;
  pt1[0] = 1.1;
  pt1[1] = 2.2;
  const auto pt2 = pt1;
  EXPECT_TRUE(pt1 == pt2);
  ITK_EXPECT_VECTOR_NEAR(pt1, pt2, 1e-10);

  VectorType vec1;
  vec1[0] = 1.1;
  vec1[1] = 2.2;
  const auto vec2 = vec1;
  EXPECT_TRUE(vec1 == vec2);
  ITK_EXPECT_VECTOR_NEAR(vec1, vec2, 1e-10);

  constexpr IndexType idx1{ 0, 1 };
  constexpr auto      idx2 = idx1;
  EXPECT_TRUE(idx1 == idx2);
  ITK_EXPECT_VECTOR_NEAR(idx1, idx2, 1e-10);

  constexpr SizeType sz1{ 0u, 1u };
  constexpr auto     sz2 = sz1;
  EXPECT_TRUE(sz1 == sz2);
  ITK_EXPECT_VECTOR_NEAR(sz1, sz2, 1e-10);
}


// cover ITK Google testing utilities
TEST(GoogleTest, TypedefsAndConstructors_Dimension3)
{

  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  PointType pt1;
  pt1[0] = 1.1;
  pt1[1] = 2.2;
  pt1[2] = 3.3;
  const auto pt2 = pt1;
  EXPECT_TRUE(pt1 == pt2);
  ITK_EXPECT_VECTOR_NEAR(pt1, pt2, 1e-10);

  VectorType vec1;
  vec1[0] = 1.1;
  vec1[1] = 2.2;
  vec1[2] = 3.3;
  const auto vec2 = vec1;
  EXPECT_TRUE(vec1 == vec2);
  ITK_EXPECT_VECTOR_NEAR(vec1, vec2, 1e-10);

  constexpr IndexType idx1{ 0, 1, 2 };
  constexpr auto      idx2 = idx1;
  EXPECT_TRUE(idx1 == idx2);
  ITK_EXPECT_VECTOR_NEAR(idx1, idx2, 1e-10);

  constexpr SizeType sz1{ 0u, 1u, 2u };
  constexpr auto     sz2 = sz1;
  EXPECT_TRUE(sz1 == sz2);
}


namespace
{
template <typename T>
::testing::AssertionResult
VectorNear(const T & val1, const T & val2)
{
  return itk::GTest::Predicate::VectorDoubleRMSPredFormat("val1", "val2", "1e-10", val1, val2, 1e-10);
}
} // namespace

TEST(GoogleTest, VectorNearComparesEveryElement)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  EXPECT_FALSE(VectorNear(IndexType{ 0, 1, 2 }, IndexType{ 0, 1, 3 }));
  EXPECT_FALSE(VectorNear(itk::Offset<3>{ 0, 1, 2 }, itk::Offset<3>{ 0, 5, 2 }));
  EXPECT_FALSE(VectorNear(SizeType{ 1, 2, 3 }, SizeType{ 1, 2, 4 }));

  itk::OptimizerParameters<double> params1(3, 0.0);
  itk::OptimizerParameters<double> params2(3, 0.0);
  params2[2] = 1.0;
  EXPECT_FALSE(VectorNear(params1, params2));
  EXPECT_TRUE(VectorNear(params1, params1));
}

TEST(GoogleTest, VectorNearRejectsDifferentLengths)
{
  const itk::OptimizerParameters<double> params2(2, 0.0);
  const itk::OptimizerParameters<double> params3(3, 0.0);
  EXPECT_FALSE(VectorNear(params2, params3));
}
