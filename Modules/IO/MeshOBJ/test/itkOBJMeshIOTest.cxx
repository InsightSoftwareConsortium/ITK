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
#include "itkOBJMeshIO.h"
#include "itkTestingMacros.h"


int
itkOBJMeshIOTest(int argc, char * argv[])
{
  if (argc != 14)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName outputFileName notAnOBJInputFileName notAnOBJOutputFileName useCompression "
                 "updatePoints updatePointData updateCells updateCellData numberOfPoints numberOfPointPixels "
                 "numberOfCells numberOfCellPixels"
              << std::endl;
    return EXIT_FAILURE;
  }


  int testStatus = EXIT_SUCCESS;

  auto objMeshIO = itk::OBJMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(objMeshIO, OBJMeshIO, MeshIOBase);

  // Create a different instance to check the base class methods
  auto objMeshIOBaseTest = itk::OBJMeshIO::New();
  testStatus = TestBaseClassMethodsMeshIO<itk::OBJMeshIO>(objMeshIOBaseTest);

  // Test reading exceptions
  std::string inputFileName = "";
  objMeshIO->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(objMeshIO->ReadMeshInformation());

  ITK_TRY_EXPECT_EXCEPTION(objMeshIO->ReadPoints(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(objMeshIO->ReadCells(nullptr));

  inputFileName = argv[3];
  ITK_TEST_EXPECT_TRUE(!objMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = "nonExistingFile.obj";
  ITK_TEST_EXPECT_TRUE(!objMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = argv[1];
  ITK_TEST_EXPECT_TRUE(objMeshIO->CanReadFile(inputFileName.c_str()));
  objMeshIO->SetFileName(inputFileName);

  // Test Set/Get methods
  auto useCompression = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(objMeshIO, UseCompression, useCompression);

  auto updatePoints = static_cast<bool>(std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(objMeshIO, UpdatePoints, updatePoints);

  auto updatePointData = static_cast<bool>(std::stoi(argv[7]));
  ITK_TEST_SET_GET_BOOLEAN(objMeshIO, UpdatePointData, updatePointData);

  auto updateCells = static_cast<bool>(std::stoi(argv[8]));
  ITK_TEST_SET_GET_BOOLEAN(objMeshIO, UpdateCells, updateCells);

  auto updateCellData = static_cast<bool>(std::stoi(argv[9]));
  ITK_TEST_SET_GET_BOOLEAN(objMeshIO, UpdateCellData, updateCellData);

  // Read the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(objMeshIO->ReadMeshInformation());

  auto numberOfPoints = static_cast<itk::OBJMeshIO::SizeValueType>(std::stoi(argv[10]));
  ITK_TEST_EXPECT_EQUAL(numberOfPoints, objMeshIO->GetNumberOfPoints());

  auto numberOfPointPixels = static_cast<itk::OBJMeshIO::SizeValueType>(std::stoi(argv[11]));
  ITK_TEST_EXPECT_EQUAL(numberOfPointPixels, objMeshIO->GetNumberOfPointPixels());

  auto numberOfCells = static_cast<itk::OBJMeshIO::SizeValueType>(std::stoi(argv[12]));
  ITK_TEST_EXPECT_EQUAL(numberOfCells, objMeshIO->GetNumberOfCells());

  auto numberOfCellPixels = static_cast<itk::OBJMeshIO::SizeValueType>(std::stoi(argv[13]));
  ITK_TEST_EXPECT_EQUAL(numberOfCellPixels, objMeshIO->GetNumberOfCellPixels());

  // Use sufficiently large buffer sizes
  itk::SizeValueType pointBufferSize = 100000;
  itk::SizeValueType pointDataBufferSize = 100000;

  itk::SizeValueType          cellBufferSize = 100000;
  const std::shared_ptr<void> pointBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(objMeshIO->GetPointComponentType(), pointBufferSize);
  const std::shared_ptr<void> pointDataBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(objMeshIO->GetPointPixelComponentType(), pointDataBufferSize);
  const std::shared_ptr<void> cellBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(objMeshIO->GetCellComponentType(), cellBufferSize);

  ITK_TRY_EXPECT_NO_EXCEPTION(objMeshIO->ReadPoints(pointBuffer.get()));
  ITK_TRY_EXPECT_NO_EXCEPTION(objMeshIO->ReadPointData(pointDataBuffer.get()));

  ITK_TRY_EXPECT_NO_EXCEPTION(objMeshIO->ReadCells(cellBuffer.get()));

  void * cellDataBuffer = nullptr;
  // Not used; empty method body; called for coverage purposes
  objMeshIO->ReadCellData(cellDataBuffer);

  // Test writing exceptions
  std::string outputFileName = "";
  objMeshIO->SetFileName(outputFileName);
  ITK_TRY_EXPECT_EXCEPTION(objMeshIO->WritePoints(pointBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(objMeshIO->WriteCells(cellBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(objMeshIO->WriteMeshInformation());

  outputFileName = argv[4];
  ITK_TEST_EXPECT_TRUE(!objMeshIO->CanWriteFile(outputFileName.c_str()));

  outputFileName = argv[2];
  ITK_TEST_EXPECT_TRUE(objMeshIO->CanWriteFile(outputFileName.c_str()));
  objMeshIO->SetFileName(outputFileName);

  // Write the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(objMeshIO->WritePoints(pointBuffer.get()));

  // Not used; empty method body; called for coverage purposes
  objMeshIO->WritePointData(pointDataBuffer.get());

  ITK_TRY_EXPECT_NO_EXCEPTION(objMeshIO->WriteCells(cellBuffer.get()));

  // Not used; empty method body; called for coverage purposes
  objMeshIO->WriteCellData(cellDataBuffer);

  ITK_TRY_EXPECT_NO_EXCEPTION(objMeshIO->WriteMeshInformation());

  // Not used; empty method body; called for coverage purposes
  objMeshIO->Write();


  // Read back the written image and check the properties
  auto readWriteByuMeshIO = itk::OBJMeshIO::New();

  readWriteByuMeshIO->SetFileName(outputFileName);
  readWriteByuMeshIO->ReadMeshInformation();

  ITK_TEST_EXPECT_EQUAL(objMeshIO->GetPointPixelType(), readWriteByuMeshIO->GetPointPixelType());
  ITK_TEST_EXPECT_EQUAL(objMeshIO->GetCellPixelType(), readWriteByuMeshIO->GetCellPixelType());

  ITK_TEST_EXPECT_EQUAL(objMeshIO->GetPointPixelComponentType(), readWriteByuMeshIO->GetPointPixelComponentType());
  ITK_TEST_EXPECT_EQUAL(objMeshIO->GetCellPixelComponentType(), readWriteByuMeshIO->GetCellPixelComponentType());

  // FIXME
  // For some reason, the number of points is different for the rh for box.obj; maybe it does not get written properly
  // ITK_TEST_EXPECT_EQUAL(objMeshIO->GetNumberOfPoints(), readWriteByuMeshIO->GetNumberOfPoints());

  // FIXME
  // For some reason, the number of cells is different for the rh for box.obj; maybe it does not get written properly
  // ITK_TEST_EXPECT_EQUAL(objMeshIO->GetNumberOfCells(), readWriteByuMeshIO->GetNumberOfCells());

  // FIXME
  // For some reason, the number of point pixels is different for the rh for bunny.obj; maybe it does not get written
  // properly ITK_TEST_EXPECT_EQUAL(objMeshIO->GetNumberOfPointPixels(), readWriteByuMeshIO->GetNumberOfPointPixels());
  ITK_TEST_EXPECT_EQUAL(objMeshIO->GetNumberOfCellPixels(), readWriteByuMeshIO->GetNumberOfCellPixels());
  ITK_TEST_EXPECT_EQUAL(objMeshIO->GetNumberOfPointPixelComponents(),
                        readWriteByuMeshIO->GetNumberOfPointPixelComponents());
  ITK_TEST_EXPECT_EQUAL(objMeshIO->GetNumberOfCellPixelComponents(),
                        readWriteByuMeshIO->GetNumberOfCellPixelComponents());

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
