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

#include "itkCommand.h"
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkTestingMacros.h"
#include "itkMesh.h"
#include "itkMeshFileTestHelper.h"

int
itkMZ3MeshIOTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputMesh";
    std::cerr << " outputMesh";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputMeshFileName = argv[1];
  const char * outputMeshFileName = argv[2];

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using MeshType = itk::Mesh<PixelType, Dimension>;
  constexpr bool isBinary = true;

  int result = EXIT_SUCCESS;

  // if (test<MeshType>(argv[1], argv[2], isBinary))
  // {
  //   std::cerr << "Failure for itk::Mesh" << std::endl;
  //   result = EXIT_FAILURE;
  // }

  // Exercise other methods to improve coverage
  itk::MZ3MeshIO::Pointer mz3MeshIO = itk::MZ3MeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mz3MeshIO, MZ3MeshIO, MeshIOBase);


  std::string fileName("NotAMZ3MeshFile.nmz3");
  ITK_TEST_EXPECT_TRUE(!mz3MeshIO->CanWriteFile(fileName.c_str()));

  fileName = "AMZ3MeshFileName.mz3";
  ITK_TEST_EXPECT_TRUE(mz3MeshIO->CanWriteFile(fileName.c_str()));


  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->ReadMeshInformation());

  // void * buffer = nullptr;

  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->ReadPoints(buffer));

  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->ReadCells(buffer));

  // fileName = "";
  // mz3MeshIO->SetFileName(fileName);
  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->WriteMeshInformation());

  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->WritePoints(buffer));

  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->WriteCells(buffer));

  // fileName = "/NonExistingDirectory/MZ3MeshFile.mz3";
  // mz3MeshIO->SetFileName(fileName);
  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->WriteMeshInformation());

  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->WritePoints(buffer));

  // ITK_TRY_EXPECT_EXCEPTION(mz3MeshIO->WriteCells(buffer));

  // // Empty functions
  // mz3MeshIO->ReadPointData(buffer);
  // mz3MeshIO->ReadCellData(buffer);

  // mz3MeshIO->WritePointData(buffer);
  // mz3MeshIO->WriteCellData(buffer);


  std::cout << "Test finished." << std::endl;
  return result;
}
