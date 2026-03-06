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

#ifndef ITK_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#endif
#include "itkBoundingBox.h"
#include "itkMath.h"
#include "itkGTest.h"

#include <iostream>
#include <cmath>

TEST(BoundingBox, EmptyBoxDefaults)
{
  using BB = itk::BoundingBox<unsigned long, 1, double>;
  auto myBox = BB::New();

  std::cout << "Testing Bounding Box" << std::endl;

  // Empty box: all bounds should be zero-initialized
  const BB::BoundsArrayType & bounds = myBox->GetBounds();
  for (unsigned int i = 0; i < bounds.Size(); ++i)
  {
    EXPECT_TRUE(itk::Math::ExactlyEquals(bounds[i], BB::CoordinateType{}));
  }
  std::cout << "Null GetBoundingBox test passed" << std::endl;

  // Empty box: center should be zero
  BB::PointType center = myBox->GetCenter();
  for (unsigned int i = 0; i < 1; ++i)
  {
    EXPECT_TRUE(itk::Math::ExactlyEquals(center[i], BB::CoordinateType{}));
  }
  std::cout << "Null GetCenter test passed" << std::endl;

  EXPECT_TRUE(itk::Math::ExactlyEquals(myBox->GetDiagonalLength2(), 0.0));
  std::cout << "Null GetDiagonalLength2 test passed" << std::endl;

  EXPECT_EQ(myBox->GetPoints(), nullptr);
  std::cout << "Null GetPoints test passed" << std::endl;
}

TEST(BoundingBox, OneDimensionalBox)
{
  using BB = itk::BoundingBox<unsigned long, 1, double>;
  auto myBox = BB::New();

  const BB::PointsContainerPointer Points = BB::PointsContainer::New();
  itk::Point<double, 1>            P;

  for (unsigned int i = 0; i < 10; ++i)
  {
    P[0] = static_cast<double>(i);
    Points->InsertElement(i, P);
  }
  std::cout << "Insert points passed" << std::endl;

  myBox->SetPoints(Points);
  EXPECT_TRUE(myBox->ComputeBoundingBox());
  std::cout << "Compute Bounding Box passed" << std::endl;

  const BB::BoundsArrayType & bounds = myBox->GetBounds();
  EXPECT_EQ(bounds[0], 0.0);
  EXPECT_EQ(bounds[1], 9.0);
  std::cout << "GetBoundingBox passed" << std::endl;

  BB::PointType center = myBox->GetCenter();
  EXPECT_EQ(center[0], 4.5);
  std::cout << "GetCenter test passed" << std::endl;

  const itk::NumericTraits<double>::AccumulateType diagonal = myBox->GetDiagonalLength2();
  EXPECT_EQ(diagonal, 81.0);
  std::cout << "GetDiagonalLength2 passed" << std::endl;

  // End with a Print.
  myBox->Print(std::cout);
}

TEST(BoundingBox, ThreeDimensionalIsInside)
{
  using CC = itk::BoundingBox<unsigned long, 3, double>;
  auto my3DBox = CC::New();

  const CC::PointsContainerPointer Points3D = CC::PointsContainer::New();

  std::cout << " Some Testing in 3D " << std::endl;

  constexpr CC::PointType::ValueType qval1[3]{ -1.0f, -1.0f, -1.0f };
  CC::PointType                      Q = qval1;
  Points3D->InsertElement(0, Q);

  CC::PointType::ValueType qval2[3] = { 1.0f, 1.0f, 1.0f };
  Q = qval2;
  Points3D->InsertElement(1, Q);
  std::cout << "Insert points passed" << std::endl;

  my3DBox->SetPoints(Points3D);
  EXPECT_TRUE(my3DBox->ComputeBoundingBox());
  std::cout << "Compute Bounding Box passed" << std::endl;

  CC::PointType::ValueType qval3[3] = { 0.0f, 0.0f, 0.0f };
  Q = qval3;
  EXPECT_TRUE(my3DBox->IsInside(Q)) << "Point " << Q << " should be inside";

  CC::PointType::ValueType qval4[3] = { 2.0f, 0.0f, 0.0f };
  Q = qval4;
  EXPECT_FALSE(my3DBox->IsInside(Q)) << "Point " << Q << " should be outside";
}

TEST(BoundingBox, ComputeCorners)
{
  using CC = itk::BoundingBox<unsigned long, 3, double>;
  auto my3DBox = CC::New();

  const CC::PointsContainerPointer Points3D = CC::PointsContainer::New();

  constexpr CC::PointType::ValueType qval1[3]{ -1.0f, -1.0f, -1.0f };
  CC::PointType                      Q = qval1;
  Points3D->InsertElement(0, Q);

  CC::PointType::ValueType qval2[3] = { 1.0f, 1.0f, 1.0f };
  Q = qval2;
  Points3D->InsertElement(1, Q);

  my3DBox->SetPoints(Points3D);
  my3DBox->ComputeBoundingBox();

  std::cout << "Testing ComputeCorners() : ";
  const auto   corners = my3DBox->ComputeCorners();
  auto         it = corners.begin();
  unsigned int j = 0;
  while (it != corners.end())
  {
    for (unsigned int i = 0; i < 3; ++i)
    {
      EXPECT_EQ((*it)[i],
                std::pow(-1.0, static_cast<double>(j / (static_cast<int>(std::pow(2.0, static_cast<double>(i)))))));
    }
    j++;
    ++it;
  }
  std::cout << "[PASSED]" << std::endl;
}

TEST(BoundingBox, DeepCopy)
{
  using CC = itk::BoundingBox<unsigned long, 3, double>;
  auto my3DBox = CC::New();

  const CC::PointsContainerPointer Points3D = CC::PointsContainer::New();

  constexpr CC::PointType::ValueType qval1[3]{ -1.0f, -1.0f, -1.0f };
  CC::PointType                      Q = qval1;
  Points3D->InsertElement(0, Q);

  CC::PointType::ValueType qval2[3] = { 1.0f, 1.0f, 1.0f };
  Q = qval2;
  Points3D->InsertElement(1, Q);

  my3DBox->SetPoints(Points3D);
  my3DBox->ComputeBoundingBox();

  constexpr double            tolerance{ 1e-10 };
  const CC::Pointer           clone = my3DBox->DeepCopy();
  const CC::BoundsArrayType & originalBounds = my3DBox->GetBounds();
  const CC::BoundsArrayType & clonedbounds = clone->GetBounds();
  for (unsigned int i = 0; i < originalBounds.Size(); ++i)
  {
    EXPECT_NEAR(originalBounds[i], clonedbounds[i], tolerance);
  }
}

#ifndef ITK_LEGACY_REMOVE
TEST(BoundingBox, LegacyGetCorners)
{
  // Expect four corners for a two-dimensional box:
  const auto boundingBox_2D = itk::BoundingBox<itk::IdentifierType, 2>::New();
  EXPECT_EQ(boundingBox_2D->GetCorners()->size(), 4u);

  // Expect eight corners for a three-dimensional box:
  const auto boundingBox_3D = itk::BoundingBox<itk::IdentifierType, 3>::New();
  EXPECT_EQ(boundingBox_3D->GetCorners()->size(), 8u);
}
#endif
