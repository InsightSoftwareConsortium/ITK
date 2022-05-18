/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkMesh.h"

#include "itkMeshFileTestHelper.h"
#include "itkTestingMacros.h"
#include "itkSWCMeshIO.h"

int
itkMeshFileReadWriteTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName outputFileName" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputFileName = argv[1];
  const char * outputFileName = argv[2];

  constexpr unsigned int dimension = 3;
  using PixelType = float;

  using MeshType = itk::Mesh<PixelType, dimension>;

  int result = EXIT_SUCCESS;

  // if (test<MeshType>(inputFileName, outputFileName, false))
  // {
  //   std::cerr << "Failure for itk::Mesh" << std::endl;
  //   result = EXIT_FAILURE;
  // }

  auto swcMeshIO = itk::SWCMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(swcMeshIO, SWCMeshIO, MeshIOBase);

  swcMeshIO->SetFileName(inputFileName);
  if (!swcMeshIO->CanReadFile(inputFileName))
  {
    std::cerr << "CanReadFile did not succeed with input file.";
    result = EXIT_FAILURE;
  }

  auto swcMeshIOOutput = itk::SWCMeshIO::New();
  swcMeshIOOutput->SetFileName(outputFileName);

  swcMeshIOOutput->SetSampleIdentifiers(swcMeshIO->GetSampleIdentifiers());
  swcMeshIOOutput->SetTypeIdentifiers(swcMeshIO->GetTypeIdentifiers());
  swcMeshIOOutput->SetRadii(swcMeshIO->GetRadii());
  swcMeshIOOutput->SetParentIdentifiers(swcMeshIO->GetParentIdentifiers());

  std::cout << "Test finished." << std::endl;
  return result;
}
