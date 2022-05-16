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

#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include "itkQuadEdgeMesh.h"
#include "itkTestingMacros.h"
#include "itkMeshFileTestHelper.h"


int
itkMeshFileReaderWriterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName outputFileName unsupportedFormatInputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  using coord = double;
  constexpr unsigned int Dimension = 3;

  using MeshType = itk::QuadEdgeMesh<coord, Dimension>;

  using ReaderType = itk::MeshFileReader<MeshType>;
  auto reader = ReaderType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reader, MeshFileReader, MeshSource);


  // Test exceptions
  std::string inputFileName = "";
  reader->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(reader->Update());

  inputFileName = "NonExistingFile.vtk";
  reader->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(reader->Update());

  inputFileName = argv[3];
  reader->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(reader->Update());

  inputFileName = argv[1];
  reader->SetFileName(inputFileName);
  ITK_TEST_SET_GET_VALUE(inputFileName, reader->GetFileName());

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  MeshType::Pointer readMesh = nullptr;
  ITK_TRY_EXPECT_NO_EXCEPTION(readMesh = itk::ReadMesh<MeshType>(inputFileName));

  std::string outputFileName = "";
  ITK_TRY_EXPECT_EXCEPTION(itk::WriteMesh(readMesh.GetPointer(), outputFileName));

  outputFileName = argv[2];
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteMesh(readMesh.GetPointer(), outputFileName));
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteMesh(readMesh, outputFileName));

  MeshType::Pointer writeReadMesh;
  ITK_TRY_EXPECT_NO_EXCEPTION(writeReadMesh = itk::ReadMesh<MeshType>(outputFileName));

  ITK_TEST_EXPECT_EQUAL(TestPointsContainer<MeshType>(readMesh->GetPoints(), writeReadMesh->GetPoints()), EXIT_SUCCESS);
  ITK_TEST_EXPECT_EQUAL(TestCellsContainer<MeshType>(readMesh->GetCells(), writeReadMesh->GetCells()), EXIT_SUCCESS);
  ITK_TEST_EXPECT_EQUAL(TestPointDataContainer<MeshType>(readMesh->GetPointData(), writeReadMesh->GetPointData()),
                        EXIT_SUCCESS);
  ITK_TEST_EXPECT_EQUAL(TestCellDataContainer<MeshType>(readMesh->GetCellData(), writeReadMesh->GetCellData()),
                        EXIT_SUCCESS);

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
