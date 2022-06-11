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

#include "itkFreeSurferAsciiMeshIO.h"
#include "itkFreeSurferBinaryMeshIO.h"
#include "itkMeshIOTestHelper.h"
#include "itkTestingMacros.h"


template <typename TMeshIO>
int
itkFreeSurferMeshIOTestHelper(typename TMeshIO::Pointer       fsMeshIO,
                              typename TMeshIO::Pointer       readWritefsMeshIO,
                              char *                          inputFileName,
                              char *                          outputFileName,
                              char *                          notAFsInputFileName,
                              char *                          notAFsOutputFileName,
                              bool                            useCompression,
                              bool                            updatePoints,
                              bool                            updatePointData,
                              bool                            updateCells,
                              bool                            updateCellData,
                              typename TMeshIO::SizeValueType numberOfPoints,
                              typename TMeshIO::SizeValueType numberOfPointPixels,
                              typename TMeshIO::SizeValueType numberOfCells,
                              typename TMeshIO::SizeValueType numberOfCellPixels)
{

  int testStatus = EXIT_SUCCESS;

  // Create a different instance to check the base class methods
  auto fsMeshIOBaseTest = TMeshIO::New();
  testStatus = TestBaseClassMethodsMeshIO<TMeshIO>(fsMeshIOBaseTest);

  // Test reading exceptions
  fsMeshIO->SetFileName("");
  ITK_TRY_EXPECT_EXCEPTION(fsMeshIO->ReadMeshInformation());

  ITK_TEST_EXPECT_TRUE(!fsMeshIO->CanReadFile(notAFsInputFileName));

  ITK_TEST_EXPECT_TRUE(!fsMeshIO->CanReadFile("nonExistingFile.fs"));

  ITK_TEST_EXPECT_TRUE(fsMeshIO->CanReadFile(inputFileName));
  fsMeshIO->SetFileName(inputFileName);

  // Test Set/Get methods
  ITK_TEST_SET_GET_BOOLEAN(fsMeshIO, UseCompression, useCompression);
  ITK_TEST_SET_GET_BOOLEAN(fsMeshIO, UpdatePoints, updatePoints);
  ITK_TEST_SET_GET_BOOLEAN(fsMeshIO, UpdateCells, updateCells);
  ITK_TEST_SET_GET_BOOLEAN(fsMeshIO, UpdatePointData, updatePointData);
  ITK_TEST_SET_GET_BOOLEAN(fsMeshIO, UpdateCellData, updateCellData);

  // Read the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->ReadMeshInformation());

  ITK_TEST_EXPECT_EQUAL(numberOfPoints, fsMeshIO->GetNumberOfPoints());
  ITK_TEST_EXPECT_EQUAL(numberOfPointPixels, fsMeshIO->GetNumberOfPointPixels());
  ITK_TEST_EXPECT_EQUAL(numberOfCells, fsMeshIO->GetNumberOfCells());
  ITK_TEST_EXPECT_EQUAL(numberOfCellPixels, fsMeshIO->GetNumberOfCellPixels());

  // Use sufficiently large buffer sizes
  itk::SizeValueType pointBufferSize = 2000;
  itk::SizeValueType pointDataBufferSize = 2000;

  itk::SizeValueType cellBufferSize = 2000;

  const std::shared_ptr<void> pointBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(fsMeshIO->GetPointComponentType(), pointBufferSize);
  const std::shared_ptr<void> pointDataBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(fsMeshIO->GetPointPixelComponentType(), pointDataBufferSize);
  const std::shared_ptr<void> cellBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(fsMeshIO->GetCellComponentType(), cellBufferSize);

  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->ReadPoints(pointBuffer.get()));
  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->ReadPointData(pointDataBuffer.get()));

  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->ReadCells(cellBuffer.get()));

  void * cellDataBuffer = nullptr;
  // Not used; empty method body; called for coverage purposes
  fsMeshIO->ReadCellData(cellDataBuffer);

  // Test writing exceptions
  fsMeshIO->SetFileName("");
  ITK_TRY_EXPECT_EXCEPTION(fsMeshIO->WritePoints(pointBuffer.get()));
  if (dynamic_cast<itk::FreeSurferBinaryMeshIO *>(fsMeshIO.GetPointer()))
  {
    ITK_TRY_EXPECT_EXCEPTION(fsMeshIO->WritePointData(pointDataBuffer.get()));
  }

  ITK_TRY_EXPECT_EXCEPTION(fsMeshIO->WriteCells(cellBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(fsMeshIO->WriteMeshInformation());

  ITK_TEST_EXPECT_TRUE(!fsMeshIO->CanWriteFile(notAFsOutputFileName));

  ITK_TEST_EXPECT_TRUE(fsMeshIO->CanWriteFile(outputFileName));
  fsMeshIO->SetFileName(outputFileName);

  // Write the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->WritePoints(pointBuffer.get()));
  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->WritePointData(pointDataBuffer.get()));

  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->WriteCells(cellBuffer.get()));

  // Not used; empty method body; called for coverage purposes
  fsMeshIO->WriteCellData(cellDataBuffer);

  ITK_TRY_EXPECT_NO_EXCEPTION(fsMeshIO->WriteMeshInformation());

  // Not used; empty method body; called for coverage purposes
  fsMeshIO->Write();


  // Read back the written image and check the properties
  readWritefsMeshIO->SetFileName(outputFileName);
  readWritefsMeshIO->ReadMeshInformation();

  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetPointPixelType(), readWritefsMeshIO->GetPointPixelType());
  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetCellPixelType(), readWritefsMeshIO->GetCellPixelType());

  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetPointPixelComponentType(), readWritefsMeshIO->GetPointPixelComponentType());
  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetCellPixelComponentType(), readWritefsMeshIO->GetCellPixelComponentType());

  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetNumberOfPoints(), readWritefsMeshIO->GetNumberOfPoints());
  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetNumberOfCells(), readWritefsMeshIO->GetNumberOfCells());

  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetNumberOfPointPixels(), readWritefsMeshIO->GetNumberOfPointPixels());
  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetNumberOfCellPixels(), readWritefsMeshIO->GetNumberOfCellPixels());
  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetNumberOfPointPixelComponents(),
                        readWritefsMeshIO->GetNumberOfPointPixelComponents());
  ITK_TEST_EXPECT_EQUAL(fsMeshIO->GetNumberOfCellPixelComponents(),
                        readWritefsMeshIO->GetNumberOfCellPixelComponents());

  return testStatus;
}

int
itkFreeSurferMeshIOTest(int argc, char * argv[])
{
  if (argc != 15)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName outputFileName notAFsInputFileName notAFsOutputFileName useCompression updatePoints "
                 "updatePointData updateCells updateCellData numberOfPoints numberOfPointPixels numberOfCells "
                 "numberOfCellPixels isBinary"
              << std::endl;
    return EXIT_FAILURE;
  }


  int testStatus = EXIT_SUCCESS;

  auto useCompression = static_cast<bool>(std::stoi(argv[5]));
  auto updatePoints = static_cast<bool>(std::stoi(argv[6]));
  auto updatePointData = static_cast<bool>(std::stoi(argv[7]));
  auto updateCells = static_cast<bool>(std::stoi(argv[8]));
  auto updateCellData = static_cast<bool>(std::stoi(argv[9]));
  auto numberOfPoints = static_cast<itk::MeshIOBase::SizeValueType>(std::stoi(argv[10]));
  auto numberOfPointPixels = static_cast<itk::MeshIOBase::SizeValueType>(std::stoi(argv[11]));
  auto numberOfCells = static_cast<itk::MeshIOBase::SizeValueType>(std::stoi(argv[12]));
  auto numberOfCellPixels = static_cast<itk::MeshIOBase::SizeValueType>(std::stoi(argv[13]));

  bool isBinary = static_cast<bool>(std::stoi(argv[14]));
  if (!isBinary)
  {
    auto fsMeshIO = itk::FreeSurferAsciiMeshIO::New();
    auto readWritefsMeshIO = itk::FreeSurferAsciiMeshIO::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(fsMeshIO, FreeSurferAsciiMeshIO, MeshIOBase);

    testStatus = itkFreeSurferMeshIOTestHelper<itk::FreeSurferAsciiMeshIO>(fsMeshIO,
                                                                           readWritefsMeshIO,
                                                                           argv[1],
                                                                           argv[2],
                                                                           argv[3],
                                                                           argv[4],
                                                                           useCompression,
                                                                           updatePoints,
                                                                           updatePointData,
                                                                           updateCells,
                                                                           updateCellData,
                                                                           numberOfPoints,
                                                                           numberOfPointPixels,
                                                                           numberOfCells,
                                                                           numberOfCellPixels);
  }
  else
  {
    auto fsMeshIO = itk::FreeSurferBinaryMeshIO::New();
    auto readWritefsMeshIO = itk::FreeSurferBinaryMeshIO::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(fsMeshIO, FreeSurferBinaryMeshIO, MeshIOBase);

    testStatus = itkFreeSurferMeshIOTestHelper<itk::FreeSurferBinaryMeshIO>(fsMeshIO,
                                                                            readWritefsMeshIO,
                                                                            argv[1],
                                                                            argv[2],
                                                                            argv[3],
                                                                            argv[4],
                                                                            useCompression,
                                                                            updatePoints,
                                                                            updatePointData,
                                                                            updateCells,
                                                                            updateCellData,
                                                                            numberOfPoints,
                                                                            numberOfPointPixels,
                                                                            numberOfCells,
                                                                            numberOfCellPixels);
  }


  std::cout << "Test finished." << std::endl;
  return testStatus;
}
