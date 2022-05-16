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

#include "itkRegularSphereMeshSource.h"
#include "itkTestingMacros.h"

#include <iostream>

int
itkRegularSphereMeshSourceTest(int, char *[])
{

  using MeshType = itk::Mesh<float, 3>;

  using SphereMeshSourceType = itk::RegularSphereMeshSource<MeshType>;

  auto mySphereMeshSource = SphereMeshSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mySphereMeshSource, RegularSphereMeshSource, MeshSource);


  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  PointType center;
  center.Fill(7.4);

  constexpr double radius = 1.5;
  const double     tolerance = 1e-5;

  VectorType scale;
  scale.Fill(radius);

  mySphereMeshSource->SetCenter(center);
  ITK_TEST_SET_GET_VALUE(center, mySphereMeshSource->GetCenter());

  unsigned int resolution = 1;
  mySphereMeshSource->SetResolution(resolution);
  ITK_TEST_SET_GET_VALUE(resolution, mySphereMeshSource->GetResolution());

  mySphereMeshSource->SetScale(scale);
  ITK_TEST_SET_GET_VALUE(scale, mySphereMeshSource->GetScale());

  mySphereMeshSource->Modified();

  ITK_TRY_EXPECT_NO_EXCEPTION(mySphereMeshSource->Update());


  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill(0);

  bool testPassed = true;

  for (unsigned int i = 0; i < myMesh->GetNumberOfPoints(); ++i)
  {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;

    const double distanceToCenter = pt.EuclideanDistanceTo(center);

    if (itk::Math::abs(distanceToCenter - radius) > tolerance)
    {
      std::cerr << "Distance to center " << distanceToCenter;
      std::cerr << " is too different from radius " << radius << std::endl;
      testPassed = false;
    }
  }

  using CellsContainerPointer = MeshType::CellsContainerPointer;
  using CellType = MeshType::CellType;

  CellsContainerPointer cells = myMesh->GetCells();

  unsigned int faceId = 0;

  MeshType::CellsContainerIterator cellsItr = cells->Begin();


  while (cellsItr != cells->End())
  {
    CellType * cellPointer = cellsItr.Value();

    if (static_cast<int>(cellPointer->GetType()) != 1)
    {
      const unsigned int numberOfPoints = cellPointer->GetNumberOfPoints();

      std::cout << "Face " << faceId << " has " << numberOfPoints << " points" << std::endl;

      if (numberOfPoints != 3)
      {
        std::cerr << "Face with wrong number of points" << std::endl;
        testPassed = false;
      }
    }

    ++cellsItr;
    ++faceId;
  }

  if (!testPassed)
  {
    std::cout << "Test FAILED! " << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test PASSED! " << std::endl;

  return EXIT_SUCCESS;
}
