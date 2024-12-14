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

#include "itkQuadEdgeMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.h"
#include "itkTestingMacros.h"

#include <iostream>

int
itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputFileName.vtk" << '\n';
    return EXIT_FAILURE;
  }

  using MeshType = itk::QuadEdgeMesh<float, 3>;

  using SphereMeshSourceType = itk::RegularSphereMeshSource<MeshType>;

  auto mySphereMeshSource = SphereMeshSourceType::New();

  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  const PointType center{};

  auto scale = itk::MakeFilled<VectorType>(1.0);

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(1);
  mySphereMeshSource->SetScale(scale);

  mySphereMeshSource->Modified();

  ITK_TRY_EXPECT_NO_EXCEPTION(mySphereMeshSource->Update());


  std::cout << "mySphereMeshSource: " << mySphereMeshSource;

  const MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt{};

  myMesh->Print(std::cout);

  for (unsigned int i = 0; i < myMesh->GetNumberOfPoints(); ++i)
  {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << '\n';
  }

  using CellsContainerPointer = MeshType::CellsContainerPointer;
  using CellType = MeshType::CellType;

  const CellsContainerPointer cells = myMesh->GetCells();

  unsigned int faceId = 0;

  for (MeshType::CellsContainerIterator cells_it = cells->Begin(); cells_it != cells->End(); ++cells_it, faceId++)
  {
    CellType * cellPointer = cells_it.Value();
    if (static_cast<int>(cellPointer->GetType()) != 1)
    {
      std::cout << "Face " << faceId << " has " << cellPointer->GetNumberOfPoints() << " points" << '\n';
    }
  }

  // Assign a value to each of the mesh points
  for (unsigned int i = 0; i < myMesh->GetNumberOfPoints(); ++i)
  {
    myMesh->SetPointData(i, 5.0);
  }

  // Assign a different value to each of the mesh cells
  for (unsigned int i = 0; i < myMesh->GetNumberOfCells(); ++i)
  {
    myMesh->SetCellData(i, 10.0);
  }

  using WriterType = itk::QuadEdgeMeshScalarDataVTKPolyDataWriter<MeshType>;

  auto writer = WriterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(writer, QuadEdgeMeshScalarDataVTKPolyDataWriter, VTKPolyDataWriter);


  const std::string cellDataName = "SphereCellData";
  writer->SetCellDataName(cellDataName);
  ITK_TEST_SET_GET_VALUE(cellDataName, writer->GetCellDataName());

  const std::string pointDataName = "SpherePointData";
  writer->SetPointDataName(pointDataName);
  ITK_TEST_SET_GET_VALUE(pointDataName, writer->GetPointDataName());

  writer->SetInput(mySphereMeshSource->GetOutput());
  writer->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << '\n';
  return EXIT_SUCCESS;
}
