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

#include "itkMZ3MeshIO.h"
#include "itkMZ3MeshIOFactory.h"

#include "itkCommand.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"
#include "itkMesh.h"
#include "itkMeshFileTestHelper.h"

int
itkMZ3MeshIOTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputMesh";
    std::cerr << " outputMesh";
    std::cerr << " outputCompressedMesh";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  char * inputMeshFileName = argv[1];
  char * outputMeshFileName = argv[2];
  char * outputCompressedMeshFileName = argv[3];

  itk::MZ3MeshIOFactory::RegisterOneFactory();

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using MeshType = itk::Mesh<PixelType, Dimension>;
  constexpr bool isBinary = true;

  int result = EXIT_SUCCESS;

  // Mesh test helper also writes outputMeshFileName
  if (test<MeshType>(inputMeshFileName, outputMeshFileName, isBinary))
  {
    std::cerr << "Failure for itk::Mesh" << std::endl;
    result = EXIT_FAILURE;
  }

  // Exercise other methods to improve coverage
  itk::MZ3MeshIO::Pointer mz3MeshIO = itk::MZ3MeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mz3MeshIO, MZ3MeshIO, MeshIOBase);


  std::string fileName("NotAMZ3MeshFile.nmz3");
  ITK_TEST_EXPECT_TRUE(!mz3MeshIO->CanWriteFile(fileName.c_str()));

  fileName = "AMZ3MeshFileName.mz3";
  ITK_TEST_EXPECT_TRUE(mz3MeshIO->CanWriteFile(fileName.c_str()));

  const auto inputMesh = itk::ReadMesh<MeshType>(inputMeshFileName);
  inputMesh->Print(std::cout);

  constexpr bool compress = true;
  itk::WriteMesh(inputMesh, outputCompressedMeshFileName, compress);

  std::cout << "Test finished." << std::endl;
  return result;
}
