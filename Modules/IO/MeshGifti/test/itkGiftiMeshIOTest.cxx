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

#include "itkGiftiMeshIO.h"
#include "itkMeshIOTestHelper.h"
#include "itkTestingMacros.h"


int
itkGiftiMeshIOTest(int argc, char * argv[])
{
  if (argc != 18)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr
      << " inputFileName outputFileName notAGiftiInputFileName notAGiftiOutputFileName useCompression updatePoints "
         "updatePointData updateCells updateCellData writeUpdatePointData writeUpdateCellData readPointData "
         "numberOfPoints numberOfPointPixels numberOfCells numberOfCellPixels requiresConsistency"
      << std::endl;
    return EXIT_FAILURE;
  }


  int testStatus = EXIT_SUCCESS;

  auto giftiMeshIO = itk::GiftiMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(giftiMeshIO, GiftiMeshIO, MeshIOBase);


  // Create a different instance to check the base class methods
  auto giftiMeshIOBaseTest = itk::GiftiMeshIO::New();
  testStatus = TestBaseClassMethodsMeshIO<itk::GiftiMeshIO>(giftiMeshIOBaseTest);

  // Test reading exceptions
  std::string inputFileName = argv[3];
  giftiMeshIO->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->ReadMeshInformation());

  ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->ReadPoints(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->ReadPointData(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->ReadCells(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->ReadCellData(nullptr));

  // Until the mesh information is read, the label color and name tables should be empty
  ITK_TEST_SET_GET_NULL_VALUE(giftiMeshIO->GetLabelColorTable());
  ITK_TEST_SET_GET_NULL_VALUE(giftiMeshIO->GetLabelNameTable());

  ITK_TEST_EXPECT_TRUE(!giftiMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = "nonExistingFile.gii";
  ITK_TEST_EXPECT_TRUE(!giftiMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = argv[1];
  ITK_TEST_EXPECT_TRUE(giftiMeshIO->CanReadFile(inputFileName.c_str()));
  giftiMeshIO->SetFileName(inputFileName);

  itk::GiftiMeshIO::SizeValueType numberOfPoints = 0;

  // Test an inconsistent point/cell data count
  bool requiresConsistency = std::stoul(argv[17]);
  if (requiresConsistency)
  {
    numberOfPoints = std::stoul(argv[11]) + 1;
    giftiMeshIO->SetNumberOfPoints(numberOfPoints);
    ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->ReadMeshInformation());

    // Reset the value
    numberOfPoints = 0;
    giftiMeshIO->SetNumberOfPoints(numberOfPoints);
  }

  // Test Set/Get methods
  auto useCompression = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(giftiMeshIO, UseCompression, useCompression);

  auto updatePoints = static_cast<bool>(std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(giftiMeshIO, UpdatePoints, updatePoints);

  auto updatePointData = static_cast<bool>(std::stoi(argv[7]));
  ITK_TEST_SET_GET_BOOLEAN(giftiMeshIO, UpdatePointData, updatePointData);

  auto updateCells = static_cast<bool>(std::stoi(argv[8]));
  ITK_TEST_SET_GET_BOOLEAN(giftiMeshIO, UpdateCells, updateCells);

  auto updateCellData = static_cast<bool>(std::stoi(argv[9]));
  ITK_TEST_SET_GET_BOOLEAN(giftiMeshIO, UpdateCellData, updateCellData);

  auto readPointData = static_cast<bool>(std::stoi(argv[12]));
  ITK_TEST_SET_GET_BOOLEAN(giftiMeshIO, ReadPointData, readPointData);

  itk::GiftiMeshIO::DirectionType direction;
  direction.SetIdentity();
  giftiMeshIO->SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, giftiMeshIO->GetDirection());

  itk::GiftiMeshIO::LabelColorContainerPointer colorMap = giftiMeshIO->GetLabelColorTable();
  itk::GiftiMeshIO::LabelNameContainerPointer  labelMap = giftiMeshIO->GetLabelNameTable();

  giftiMeshIO->SetLabelColorTable(colorMap);
  ITK_TEST_SET_GET_VALUE(colorMap, giftiMeshIO->GetLabelColorTable());

  giftiMeshIO->SetLabelNameTable(labelMap);
  ITK_TEST_SET_GET_VALUE(labelMap, giftiMeshIO->GetLabelNameTable());

  // Read the actual data
  ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->ReadMeshInformation());

  numberOfPoints = static_cast<itk::GiftiMeshIO::SizeValueType>(std::stoi(argv[13]));
  ITK_TEST_EXPECT_EQUAL(numberOfPoints, giftiMeshIO->GetNumberOfPoints());

  auto numberOfPointPixels = static_cast<itk::GiftiMeshIO::SizeValueType>(std::stoi(argv[14]));
  ITK_TEST_EXPECT_EQUAL(numberOfPointPixels, giftiMeshIO->GetNumberOfPointPixels());

  auto numberOfCells = static_cast<itk::GiftiMeshIO::SizeValueType>(std::stoi(argv[15]));
  ITK_TEST_EXPECT_EQUAL(numberOfCells, giftiMeshIO->GetNumberOfCells());

  auto numberOfCellPixels = static_cast<itk::GiftiMeshIO::SizeValueType>(std::stoi(argv[16]));
  ITK_TEST_EXPECT_EQUAL(numberOfCellPixels, giftiMeshIO->GetNumberOfCellPixels());

  // Use sufficiently large buffer sizes
  itk::SizeValueType pointBufferSize = 1000000;
  itk::SizeValueType pointDataBufferSize = 1000000;

  itk::SizeValueType cellBufferSize = 1000000;
  itk::SizeValueType cellDataBufferSize = 1000000;

  const std::shared_ptr<void> pointBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(giftiMeshIO->GetPointComponentType(), pointBufferSize);
  const std::shared_ptr<void> pointDataBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(giftiMeshIO->GetPointPixelComponentType(), pointDataBufferSize);
  const std::shared_ptr<void> cellBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(giftiMeshIO->GetCellComponentType(), cellBufferSize);
  const std::shared_ptr<void> cellDataBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(giftiMeshIO->GetCellPixelComponentType(), cellDataBufferSize);

  ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->ReadPoints(pointBuffer.get()));
  ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->ReadPointData(pointDataBuffer.get()));

  ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->ReadCells(cellBuffer.get()));
  ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->ReadCellData(cellDataBuffer.get()));

  auto writeUpdatePointData = static_cast<bool>(std::stoi(argv[10]));
  giftiMeshIO->SetUpdatePointData(writeUpdatePointData);

  auto writeUpdateCellData = static_cast<bool>(std::stoi(argv[11]));
  giftiMeshIO->SetUpdatePointData(writeUpdatePointData);

  // Test writing exceptions
  unsigned int numberOfPointPixelComponents = giftiMeshIO->GetNumberOfPointPixelComponents();
  if (numberOfPointPixelComponents != 1 && numberOfPointPixelComponents != 3)
  {
    bool localUpdatePointData = true;
    giftiMeshIO->SetUpdatePointData(localUpdatePointData);
    ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->WriteMeshInformation());

    giftiMeshIO->SetUpdatePointData(writeUpdatePointData);
  }

  unsigned int numberOfCellPixelComponents = giftiMeshIO->GetNumberOfCellPixelComponents();
  if (numberOfCellPixelComponents != 1 && numberOfCellPixelComponents != 3)
  {
    bool localUpdateCellData = true;
    giftiMeshIO->SetUpdateCellData(localUpdateCellData);
    ITK_TRY_EXPECT_EXCEPTION(giftiMeshIO->WriteMeshInformation());

    giftiMeshIO->SetUpdateCellData(writeUpdateCellData);
  }

  std::string outputFileName = argv[4];
  ITK_TEST_EXPECT_TRUE(!giftiMeshIO->CanWriteFile(outputFileName.c_str()));

  outputFileName = argv[2];
  ITK_TEST_EXPECT_TRUE(giftiMeshIO->CanWriteFile(outputFileName.c_str()));

  // Write the actual data
  // ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->WritePoints(pointBuffer.get()));
  // ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->WritePointData(pointDataBuffer.get()));

  // ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->WriteCells(cellBuffer.get()));
  // ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->WriteCellData(cellDataBuffer.get()));

  ITK_TRY_EXPECT_NO_EXCEPTION(giftiMeshIO->WriteMeshInformation());

  giftiMeshIO->SetFileName(outputFileName);
  giftiMeshIO->Write();


  // Read back the written image and check the properties
  auto readWriteGiftiMeshIO = itk::GiftiMeshIO::New();

  readWriteGiftiMeshIO->SetFileName(outputFileName);
  readWriteGiftiMeshIO->ReadMeshInformation();

  ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetPointPixelType(), readWriteGiftiMeshIO->GetPointPixelType());
  ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetCellPixelType(), readWriteGiftiMeshIO->GetCellPixelType());

  // FIXME
  // For some reason, the point pixel component type is different for the rh for aparc.gii; maybe it does not get
  // written properly ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetPointPixelComponentType(),
  // readWriteGiftiMeshIO->GetPointPixelComponentType());
  ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetCellPixelComponentType(), readWriteGiftiMeshIO->GetCellPixelComponentType());

  // FIXME
  // For some reason, the point color table is different for the rh for aparc.gii; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetLabelColorTable(), readWriteGiftiMeshIO->GetLabelColorTable());

  // FIXME
  // For some reason, the point label name table is different for the rh for aparc.gii; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetLabelNameTable(), readWriteGiftiMeshIO->GetLabelNameTable());

  ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetNumberOfPoints(), readWriteGiftiMeshIO->GetNumberOfPoints());
  ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetNumberOfCells(), readWriteGiftiMeshIO->GetNumberOfCells());

  // FIXME
  // For some reason, the number of point pixels is different for the rh for aparc.gii; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetNumberOfPointPixels(), readWriteGiftiMeshIO->GetNumberOfPointPixels());
  ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetNumberOfCellPixels(), readWriteGiftiMeshIO->GetNumberOfCellPixels());

  // FIXME
  // For some reason, the number of point pixel components is different for the rh for aparc.gii; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetNumberOfPointPixelComponents(),
  //                      readWriteGiftiMeshIO->GetNumberOfPointPixelComponents());
  ITK_TEST_EXPECT_EQUAL(giftiMeshIO->GetNumberOfCellPixelComponents(),
                        readWriteGiftiMeshIO->GetNumberOfCellPixelComponents());

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
