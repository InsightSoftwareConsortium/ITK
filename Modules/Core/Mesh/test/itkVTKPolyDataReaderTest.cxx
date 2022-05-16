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

#include "itkVTKPolyDataReader.h"
#include "itkTestingMacros.h"

#include <iostream>

int
itkVTKPolyDataReaderTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  using MeshType = itk::Mesh<float, 3>;
  using ReaderType = itk::VTKPolyDataReader<MeshType>;

  auto polyDataReader = ReaderType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(polyDataReader, VTKPolyDataReader, MeshSource);


  using PointType = ReaderType::PointType;

  std::string filename = argv[1];
  polyDataReader->SetFileName(filename);
  ITK_TEST_SET_GET_VALUE(filename, polyDataReader->GetFileName());

  ITK_TRY_EXPECT_NO_EXCEPTION(polyDataReader->Update());


  std::cout << "Version: " << polyDataReader->GetVersion() << std::endl;
  std::cout << "Header: " << polyDataReader->GetHeader() << std::endl;

  MeshType::Pointer mesh = polyDataReader->GetOutput();

  PointType point;

  unsigned int numberOfPoints = mesh->GetNumberOfPoints();
  unsigned int numberOfCells = mesh->GetNumberOfCells();

  std::cout << "numberOfPoints= " << numberOfPoints << std::endl;
  std::cout << "numberOfCells= " << numberOfCells << std::endl;

  if (!numberOfPoints)
  {
    std::cerr << "ERROR: numberOfPoints= " << numberOfPoints << std::endl;
    return EXIT_FAILURE;
  }

  if (!numberOfCells)
  {
    std::cerr << "ERROR: numberOfCells= " << numberOfCells << std::endl;
    return EXIT_FAILURE;
  }

  for (unsigned int i = 0; i < numberOfPoints; ++i)
  {
    mesh->GetPoint(i, &point);
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
