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
#include "itkVTKPolyDataReader.h"
#include "itkTestingMacros.h"

#include <iostream>

int
itkVTKPolyDataReaderQuadEdgeMeshTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFilename" << '\n';
    return EXIT_FAILURE;
  }

  using MeshType = itk::QuadEdgeMesh<float, 3>;
  using ReaderType = itk::VTKPolyDataReader<MeshType>;

  auto polyDataReader = ReaderType::New();

  using PointType = ReaderType::PointType;

  polyDataReader->SetFileName(argv[1]);

  try
  {
    polyDataReader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error during Update() " << '\n';
    std::cerr << excp << '\n';
  }

  std::cout << "polyDataReader:" << '\n';
  std::cout << polyDataReader << '\n';

  const MeshType::Pointer mesh = polyDataReader->GetOutput();

  std::cout << "Using following MeshType :";
  std::cout << mesh->GetNameOfClass() << '\n';

  const PointType point{};


  std::cout << "Testing itk::VTKPolyDataReader" << '\n';

  const unsigned int numberOfPoints = mesh->GetNumberOfPoints();
  const unsigned int numberOfCells = mesh->GetNumberOfCells();

  std::cout << "numberOfPoints= " << numberOfPoints << '\n';
  std::cout << "numberOfCells= " << numberOfCells << '\n';

  if (!numberOfPoints)
  {
    std::cerr << "ERROR: numberOfPoints= " << numberOfPoints << '\n';
    return EXIT_FAILURE;
  }

  if (!numberOfCells)
  {
    std::cerr << "ERROR: numberOfCells= " << numberOfCells << '\n';
    return EXIT_FAILURE;
  }

  for (unsigned int i = 0; i < numberOfPoints; ++i)
  {
    // mesh->GetPoint(i, &point);
    // std::cout << "Point[" << i << "]: " << point << '\n';
  }

  std::cout << "Test passed" << '\n';
  return EXIT_SUCCESS;
}
