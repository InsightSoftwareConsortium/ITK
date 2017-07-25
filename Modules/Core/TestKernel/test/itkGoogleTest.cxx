/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkGTest.h"
#include "itkMath.h"

// Minimal test to verify Google Test works
TEST(GoogleTest,t1) {
  void *ptr = NULL;
  ASSERT_TRUE((ptr == ITK_NULLPTR));
  EXPECT_TRUE((ptr == ITK_NULLPTR));
}

// cover ITK Google testing utilities
TEST(GoogleTest,TypedefsAndConstructors_Dimension2) {

  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  PointType pt1;
  pt1[0] = 1.1;
  pt1[1] = 2.2;
  const PointType pt2 = MakePoint(1.1, 2.2);
  EXPECT_TRUE(pt1 == pt2);
  EXPECT_VECTOR_NEAR(pt1, pt2, 1e-10);
  EXPECT_VECTOR_NEAR(pt1, MakePoint(1.1, 2.2), 1e-10);

  VectorType vec1;
  vec1[0] = 1.1;
  vec1[1] = 2.2;
  const VectorType vec2 = MakeVector(1.1, 2.2);
  EXPECT_TRUE(vec1 == vec2);
  EXPECT_VECTOR_NEAR(vec1, vec2, 1e-10);
  EXPECT_VECTOR_NEAR(vec1, MakeVector(1.1, 2.2), 1e-10);

  const IndexType idx1 = {{0,1}};
  const IndexType idx2 = MakeIndex(0,1);
  EXPECT_TRUE(idx1 == idx2);
  EXPECT_VECTOR_NEAR(idx1, idx2, 1e-10);
  EXPECT_VECTOR_NEAR(idx1, MakeIndex(0,1), 1e-10);

  const SizeType sz1 = {{0u,1u}};
  const SizeType sz2 = MakeSize(0u,1u);
  EXPECT_TRUE(sz1 == sz2);
  EXPECT_VECTOR_NEAR(sz1, sz2, 1e-10);
  EXPECT_VECTOR_NEAR(sz1, MakeSize(0u,1u), 1e-10);
}


// cover ITK Google testing utilities
TEST(GoogleTest,TypedefsAndConstructors_Dimension3) {

  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  PointType pt1;
  pt1[0] = 1.1;
  pt1[1] = 2.2;
  pt1[2] = 3.3;
  const PointType pt2 = MakePoint(1.1, 2.2, 3.3);
  EXPECT_TRUE(pt1 == pt2);
  EXPECT_VECTOR_NEAR(pt1, pt2, 1e-10);
  EXPECT_VECTOR_NEAR(pt1,  MakePoint(1.1, 2.2, 3.3), 1e-10);

  VectorType vec1;
  vec1[0] = 1.1;
  vec1[1] = 2.2;
  vec1[2] = 3.3;
  const VectorType vec2 = MakeVector(1.1, 2.2, 3.3);
  EXPECT_TRUE(vec1 == vec2);
  EXPECT_VECTOR_NEAR(vec1, vec2, 1e-10);
  EXPECT_VECTOR_NEAR(vec1, MakeVector(1.1, 2.2, 3.3), 1e-10);

  const IndexType idx1 = {{0,1,2}};
  const IndexType idx2 = MakeIndex(0,1,2);
  EXPECT_TRUE(idx1 == idx2);
  EXPECT_VECTOR_NEAR(idx1, idx2, 1e-10);

  const SizeType sz1 = {{0u,1u,2u}};
  const SizeType sz2 = MakeSize(0u,1u,2u);
  EXPECT_TRUE(sz1 == sz2);
  EXPECT_VECTOR_NEAR(sz1, MakeSize(0u,1u,2u), 1e-10);
}
