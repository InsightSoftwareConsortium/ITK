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

#include "itkMeshFileTestHelper.h"
#include "itkBYUMeshIO.h"
#include "itkTestingMacros.h"

int
itkMeshFileReadWriteTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  bool isBinary = (argc > 3);

  constexpr unsigned int dimension = 3;
  using PixelType = float;

  using MeshType = itk::Mesh<PixelType, dimension>;
  using QEMeshType = itk::QuadEdgeMesh<PixelType, dimension>;

  int result = EXIT_SUCCESS;

  if (test<MeshType>(argv[1], argv[2], isBinary))
  {
    std::cerr << "Failure for itk::Mesh" << std::endl;
    result = EXIT_FAILURE;
  }
  if (test<QEMeshType>(argv[1], argv[2], isBinary))
  {
    std::cerr << "Failure for itk::QuadEdgeMesh" << std::endl;
    result = EXIT_FAILURE;
  }


  // Exercise other methods to improve coverage
  itk::BYUMeshIO::Pointer byuMeshIO = itk::BYUMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(byuMeshIO, BYUMeshIO, MeshIOBase);


  std::string fileName("NotABYUMeshFile.nbyu");
  ITK_TEST_EXPECT_TRUE(!byuMeshIO->CanWriteFile(fileName.c_str()));

  fileName = "ABYUMeshFileName.byu";
  ITK_TEST_EXPECT_TRUE(byuMeshIO->CanWriteFile(fileName.c_str()));


  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->ReadMeshInformation());

  void * buffer = nullptr;

  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->ReadPoints(buffer));

  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->ReadCells(buffer));

  fileName = "";
  byuMeshIO->SetFileName(fileName);
  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WriteMeshInformation());

  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WritePoints(buffer));

  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WriteCells(buffer));

  fileName = "/NonExistingDirectory/BYUMeshFile.byu";
  byuMeshIO->SetFileName(fileName);
  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WriteMeshInformation());

  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WritePoints(buffer));

  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WriteCells(buffer));

  // Empty functions
  byuMeshIO->ReadPointData(buffer);
  byuMeshIO->ReadCellData(buffer);

  byuMeshIO->WritePointData(buffer);
  byuMeshIO->WriteCellData(buffer);


  std::cout << "Test finished." << std::endl;
  return result;
}
