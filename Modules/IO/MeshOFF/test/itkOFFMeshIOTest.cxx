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

#include "itkMeshIOTestHelper.h"
#include "itkOFFMeshIO.h"
#include "itkTestingMacros.h"


int
itkOFFMeshIOTest(int argc, char * argv[])
{
  if (argc != 14)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName outputFileName notAnOFFInputFileName notAnOFFOutputFileName useCompression "
                 "updatePoints updatePointData updateCells updateCellData numberOfPoints numberOfPointPixels "
                 "numberOfCells numberOfCellPixels"
              << std::endl;
    return EXIT_FAILURE;
  }


  int testStatus = EXIT_SUCCESS;

  auto offMeshIO = itk::OFFMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(offMeshIO, OFFMeshIO, MeshIOBase);

  // Create a different instance to check the base class methods
  auto offMeshIOBaseTest = itk::OFFMeshIO::New();
  testStatus = TestBaseClassMethodsMeshIO<itk::OFFMeshIO>(offMeshIOBaseTest);

  // Test reading exceptions
  std::string inputFileName = "";
  offMeshIO->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(offMeshIO->ReadMeshInformation());

  inputFileName = argv[3];
  ITK_TEST_EXPECT_TRUE(!offMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = "nonExistingFile.off";
  ITK_TEST_EXPECT_TRUE(!offMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = argv[1];
  ITK_TEST_EXPECT_TRUE(offMeshIO->CanReadFile(inputFileName.c_str()));
  offMeshIO->SetFileName(inputFileName);

  // Test Set/Get methods
  auto useCompression = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(offMeshIO, UseCompression, useCompression);

  auto updatePoints = static_cast<bool>(std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(offMeshIO, UpdatePoints, updatePoints);

  auto updatePointData = static_cast<bool>(std::stoi(argv[7]));
  ITK_TEST_SET_GET_BOOLEAN(offMeshIO, UpdatePointData, updatePointData);

  auto updateCells = static_cast<bool>(std::stoi(argv[8]));
  ITK_TEST_SET_GET_BOOLEAN(offMeshIO, UpdateCells, updateCells);

  auto updateCellData = static_cast<bool>(std::stoi(argv[9]));
  ITK_TEST_SET_GET_BOOLEAN(offMeshIO, UpdateCellData, updateCellData);

  ITK_TEST_EXPECT_TRUE(offMeshIO->CanReadFile(inputFileName.c_str()));

  // Read the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(offMeshIO->ReadMeshInformation());

  auto numberOfPoints = static_cast<itk::OFFMeshIO::SizeValueType>(std::stoi(argv[10]));
  ITK_TEST_EXPECT_EQUAL(numberOfPoints, offMeshIO->GetNumberOfPoints());

  auto numberOfPointPixels = static_cast<itk::OFFMeshIO::SizeValueType>(std::stoi(argv[11]));
  ITK_TEST_EXPECT_EQUAL(numberOfPointPixels, offMeshIO->GetNumberOfPointPixels());

  auto numberOfCells = static_cast<itk::OFFMeshIO::SizeValueType>(std::stoi(argv[12]));
  ITK_TEST_EXPECT_EQUAL(numberOfCells, offMeshIO->GetNumberOfCells());

  auto numberOfCellPixels = static_cast<itk::OFFMeshIO::SizeValueType>(std::stoi(argv[13]));
  ITK_TEST_EXPECT_EQUAL(numberOfCellPixels, offMeshIO->GetNumberOfCellPixels());

  // Use sufficiently large buffer sizes
  itk::SizeValueType pointBufferSize = 1000;
  itk::SizeValueType cellBufferSize = 1000;

  const std::shared_ptr<void> pointBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(offMeshIO->GetPointComponentType(), pointBufferSize);
  const std::shared_ptr<void> cellBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(offMeshIO->GetCellComponentType(), cellBufferSize);

  ITK_TRY_EXPECT_NO_EXCEPTION(offMeshIO->ReadPoints(pointBuffer.get()));

  void * pointDataBuffer = nullptr;
  // Not used; empty method body; called for coverage purposes
  offMeshIO->ReadPointData(pointDataBuffer);

  ITK_TRY_EXPECT_NO_EXCEPTION(offMeshIO->ReadCells(cellBuffer.get()));

  void * cellDataBuffer = nullptr;
  // Not used; empty method body; called for coverage purposes
  offMeshIO->ReadCellData(cellDataBuffer);

  // Test writing exceptions
  std::string outputFileName = "";
  offMeshIO->SetFileName(outputFileName);
  ITK_TRY_EXPECT_EXCEPTION(offMeshIO->WritePoints(pointBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(offMeshIO->WriteCells(cellBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(offMeshIO->WriteMeshInformation());

  outputFileName = argv[4];
  ITK_TEST_EXPECT_TRUE(!offMeshIO->CanWriteFile(outputFileName.c_str()));

  outputFileName = argv[2];
  ITK_TEST_EXPECT_TRUE(offMeshIO->CanWriteFile(outputFileName.c_str()));
  offMeshIO->SetFileName(outputFileName);

  // Write the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(offMeshIO->WritePoints(pointBuffer.get()));

  // Not used; empty method body; called for coverage purposes
  offMeshIO->WritePointData(pointDataBuffer);

  ITK_TRY_EXPECT_NO_EXCEPTION(offMeshIO->WriteCells(cellBuffer.get()));

  // Not used; empty method body; called for coverage purposes
  offMeshIO->WriteCellData(cellDataBuffer);

  ITK_TRY_EXPECT_NO_EXCEPTION(offMeshIO->WriteMeshInformation());

  // Not used; empty method body; called for coverage purposes
  offMeshIO->Write();


  // Read back the written image and check the properties
  auto readWriteByuMeshIO = itk::OFFMeshIO::New();

  readWriteByuMeshIO->SetFileName(outputFileName);
  readWriteByuMeshIO->ReadMeshInformation();

  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetPointPixelType(), readWriteByuMeshIO->GetPointPixelType());
  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetCellPixelType(), readWriteByuMeshIO->GetCellPixelType());

  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetPointPixelComponentType(), readWriteByuMeshIO->GetPointPixelComponentType());
  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetCellPixelComponentType(), readWriteByuMeshIO->GetCellPixelComponentType());

  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetNumberOfPoints(), readWriteByuMeshIO->GetNumberOfPoints());
  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetNumberOfCells(), readWriteByuMeshIO->GetNumberOfCells());
  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetNumberOfPointPixels(), readWriteByuMeshIO->GetNumberOfPointPixels());
  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetNumberOfCellPixels(), readWriteByuMeshIO->GetNumberOfCellPixels());
  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetNumberOfPointPixelComponents(),
                        readWriteByuMeshIO->GetNumberOfPointPixelComponents());
  ITK_TEST_EXPECT_EQUAL(offMeshIO->GetNumberOfCellPixelComponents(),
                        readWriteByuMeshIO->GetNumberOfCellPixelComponents());

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
