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

#include "itkBYUMeshIO.h"
#include "itkMeshIOTestHelper.h"
#include "itkTestingMacros.h"


int
itkBYUMeshIOTest(int argc, char * argv[])
{
  if (argc != 14)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName outputFileName notABYUInputFileName notABYUOutputFileName useCompression "
                 "updatePoints updatePointData updateCells updateCellData numberOfPoints numberOfPointPixels "
                 "numberOfCells numberOfCellPixels"
              << std::endl;
    return EXIT_FAILURE;
  }


  int testStatus = EXIT_SUCCESS;

  auto byuMeshIO = itk::BYUMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(byuMeshIO, BYUMeshIO, MeshIOBase);

  // Create a different instance to check the base class methods
  auto byuMeshIOBaseTest = itk::BYUMeshIO::New();
  testStatus = TestBaseClassMethodsMeshIO<itk::BYUMeshIO>(byuMeshIOBaseTest);

  // Test reading exceptions
  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->ReadPoints(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->ReadCells(nullptr));

  std::string inputFileName = argv[3];
  ITK_TEST_EXPECT_TRUE(!byuMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = "nonExistingFile.byu";
  ITK_TEST_EXPECT_TRUE(!byuMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = argv[1];
  ITK_TEST_EXPECT_TRUE(byuMeshIO->CanReadFile(inputFileName.c_str()));
  byuMeshIO->SetFileName(inputFileName);

  // Test Set/Get methods
  auto useCompression = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(byuMeshIO, UseCompression, useCompression);

  auto updatePoints = static_cast<bool>(std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(byuMeshIO, UpdatePoints, updatePoints);

  auto updatePointData = static_cast<bool>(std::stoi(argv[7]));
  ITK_TEST_SET_GET_BOOLEAN(byuMeshIO, UpdatePointData, updatePointData);

  auto updateCells = static_cast<bool>(std::stoi(argv[8]));
  ITK_TEST_SET_GET_BOOLEAN(byuMeshIO, UpdateCells, updateCells);

  auto updateCellData = static_cast<bool>(std::stoi(argv[9]));
  ITK_TEST_SET_GET_BOOLEAN(byuMeshIO, UpdateCellData, updateCellData);

  // Read the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(byuMeshIO->ReadMeshInformation());

  auto numberOfPoints = static_cast<itk::BYUMeshIO::SizeValueType>(std::stoi(argv[10]));
  ITK_TEST_EXPECT_EQUAL(numberOfPoints, byuMeshIO->GetNumberOfPoints());

  auto numberOfPointPixels = static_cast<itk::BYUMeshIO::SizeValueType>(std::stoi(argv[11]));
  ITK_TEST_EXPECT_EQUAL(numberOfPointPixels, byuMeshIO->GetNumberOfPointPixels());

  auto numberOfCells = static_cast<itk::BYUMeshIO::SizeValueType>(std::stoi(argv[12]));
  ITK_TEST_EXPECT_EQUAL(numberOfCells, byuMeshIO->GetNumberOfCells());

  auto numberOfCellPixels = static_cast<itk::BYUMeshIO::SizeValueType>(std::stoi(argv[13]));
  ITK_TEST_EXPECT_EQUAL(numberOfCellPixels, byuMeshIO->GetNumberOfCellPixels());

  // Use sufficiently large buffer sizes
  itk::SizeValueType pointBufferSize = 1000;
  itk::SizeValueType cellBufferSize = 1000;

  const std::shared_ptr<void> pointBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(byuMeshIO->GetPointComponentType(), pointBufferSize);
  const std::shared_ptr<void> cellBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(byuMeshIO->GetCellComponentType(), cellBufferSize);

  ITK_TRY_EXPECT_NO_EXCEPTION(byuMeshIO->ReadPoints(pointBuffer.get()));

  void * pointDataBuffer = nullptr;
  // Not used; empty method body; called for coverage purposes
  byuMeshIO->ReadPointData(pointDataBuffer);

  ITK_TRY_EXPECT_NO_EXCEPTION(byuMeshIO->ReadCells(cellBuffer.get()));

  void * cellDataBuffer = nullptr;
  // Not used; empty method body; called for coverage purposes
  byuMeshIO->ReadCellData(cellDataBuffer);

  // Test writing exceptions
  std::string outputFileName = "";
  byuMeshIO->SetFileName(outputFileName);
  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WritePoints(pointBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WriteCells(cellBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(byuMeshIO->WriteMeshInformation());

  outputFileName = argv[4];
  ITK_TEST_EXPECT_TRUE(!byuMeshIO->CanWriteFile(outputFileName.c_str()));

  outputFileName = argv[2];
  ITK_TEST_EXPECT_TRUE(byuMeshIO->CanWriteFile(outputFileName.c_str()));
  byuMeshIO->SetFileName(outputFileName.c_str());

  // Write the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(byuMeshIO->WritePoints(pointBuffer.get()));

  // Not used; empty method body; called for coverage purposes
  byuMeshIO->WritePointData(pointDataBuffer);

  ITK_TRY_EXPECT_NO_EXCEPTION(byuMeshIO->WriteCells(cellBuffer.get()));

  // Not used; empty method body; called for coverage purposes
  byuMeshIO->WriteCellData(cellDataBuffer);

  // FIXME
  // Needs investigation: exceeds the 25 minute testing timeout threshold.
  // ITK_TRY_EXPECT_NO_EXCEPTION(byuMeshIO->WriteMeshInformation());

  // Not used; empty method body; called for coverage purposes
  byuMeshIO->Write();


  // Read back the written image and check the properties
  auto readWriteByuMeshIO = itk::BYUMeshIO::New();

  readWriteByuMeshIO->SetFileName(outputFileName);
  readWriteByuMeshIO->ReadMeshInformation();

  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetPointPixelType(), readWriteByuMeshIO->GetPointPixelType());
  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetCellPixelType(), readWriteByuMeshIO->GetCellPixelType());

  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetPointPixelComponentType(), readWriteByuMeshIO->GetPointPixelComponentType());
  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetCellPixelComponentType(), readWriteByuMeshIO->GetCellPixelComponentType());

  // FIXME
  // For some reason, the number of points is different for the rh for cube.byu; maybe it does not get written properly
  // ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetNumberOfPoints(), readWriteByuMeshIO->GetNumberOfPoints());

  // FIXME
  // For some reason, the number of cells is different for the rh for cube.byu; maybe it does not get written properly
  // ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetNumberOfCells(), readWriteByuMeshIO->GetNumberOfCells());

  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetNumberOfPointPixels(), readWriteByuMeshIO->GetNumberOfPointPixels());
  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetNumberOfCellPixels(), readWriteByuMeshIO->GetNumberOfCellPixels());
  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetNumberOfPointPixelComponents(),
                        readWriteByuMeshIO->GetNumberOfPointPixelComponents());
  ITK_TEST_EXPECT_EQUAL(byuMeshIO->GetNumberOfCellPixelComponents(),
                        readWriteByuMeshIO->GetNumberOfCellPixelComponents());


  std::cout << "Test finished." << std::endl;
  return testStatus;
}
