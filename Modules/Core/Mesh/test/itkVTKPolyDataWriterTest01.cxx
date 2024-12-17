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

#include "itkVTKPolyDataWriter.h"
#include "itkTestingMacros.h"

int
itkVTKPolyDataWriterTest01(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int PointDimension = 3;

  using PointType = float;

  using MeshType = itk::Mesh<PointType, PointDimension>;

  using CellTraits = MeshType::CellTraits;
  using CellInterfaceType = itk::CellInterface<PointType, CellTraits>;
  using TriangleCellType = itk::TriangleCell<CellInterfaceType>;
  using LineCellType = itk::LineCell<CellInterfaceType>;

  using WriterType = itk::VTKPolyDataWriter<MeshType>;

  auto mesh = MeshType::New();

  constexpr unsigned int numberOfPoints = 4;
  constexpr unsigned int numberOfCells = 9;

  constexpr float rawPoints[12] = { 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0 };

  constexpr unsigned long rawCells[24] = { 0, 2, 1, 0, 1, 3, 0, 3, 2, 1, 2, 3, 0, 1, 0, 2, 0, 3, 1, 2, 1, 3, 2, 3 };

  mesh->GetPoints()->Reserve(numberOfPoints);
  mesh->GetCells()->Reserve(numberOfCells);

  MeshType::PointType point;

  for (unsigned int i = 0; i < numberOfPoints; ++i)
  {
    point[0] = rawPoints[3 * i];
    point[1] = rawPoints[3 * i + 1];
    point[2] = rawPoints[3 * i + 2];
    mesh->SetPoint(i, point);
  }

  MeshType::PointIdentifier pointIds[3];

  MeshType::CellAutoPointer cell;

  for (unsigned int i = 0; i < 4; ++i)
  {
    pointIds[0] = rawCells[3 * i];
    pointIds[1] = rawCells[3 * i + 1];
    pointIds[2] = rawCells[3 * i + 2];

    auto * triangle = new TriangleCellType;
    triangle->SetPointIds(pointIds);
    cell.TakeOwnership(triangle);
    mesh->SetCell(i, cell);
  }
  for (unsigned int i = 4; i < 10; ++i)
  {
    pointIds[0] = rawCells[12 + 2 * (i - 4)];
    pointIds[1] = rawCells[12 + 2 * (i - 4) + 1];

    auto * line = new LineCellType;
    line->SetPointIds(pointIds);
    cell.TakeOwnership(line);
    mesh->SetCell(i, cell);
  }

  auto writer = WriterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(writer, VTKPolyDataWriter, Object);


  writer->SetInput(mesh);
  const std::string inputFileName = argv[1];
  writer->SetFileName(inputFileName);
  ITK_TEST_SET_GET_VALUE(inputFileName, writer->GetFileName());

  writer->Write();


  return EXIT_SUCCESS;
}
