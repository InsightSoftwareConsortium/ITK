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
#include "itkVTKPolyDataWriter.h"
#include "itkTestingMacros.h"

#include <iostream>

int
itkVTKPolyDataWriterTest02(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputFileName " << std::endl;
    return EXIT_FAILURE;
  }


  using MeshType = itk::Mesh<float, 3>;

  using SphereMeshSourceType = itk::RegularSphereMeshSource<MeshType>;

  auto mySphereMeshSource = SphereMeshSourceType::New();

  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  PointType center;
  center.Fill(7.4);

  constexpr double radius = 1.5;
  const double     tolerance = 1e-5;

  VectorType scale;
  scale.Fill(radius);

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(1);
  mySphereMeshSource->SetScale(scale);

  mySphereMeshSource->Modified();

  ITK_TRY_EXPECT_NO_EXCEPTION(mySphereMeshSource->Update());


  std::cout << "mySphereMeshSource: " << mySphereMeshSource;

  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill(0.);

  bool testPassed = true;

  std::cout << "Testing itk::RegularSphereMeshSource " << std::endl;

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


  using WriterType = itk::VTKPolyDataWriter<MeshType>;

  auto writer = WriterType::New();
  writer->SetInput(myMesh);
  writer->SetFileName(argv[1]);
  writer->Write();


  if (!testPassed)
  {
    std::cout << "Test FAILED! " << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test PASSED! " << std::endl;

  return EXIT_SUCCESS;
}
