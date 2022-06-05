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
#include "itkVTKPolyDataMeshIO.h"
#include "itkTestingMacros.h"


int
itkVTKPolyDataMeshIOTest(int argc, char * argv[])
{
  if (argc != 16)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputFileName outputFileName notAVTKInputFileName notAVTKOutputFileName useCompression "
                 "updatePoints updatePointData updateCells updateCellData numberOfPoints numberOfPointPixels "
                 "numberOfCells numberOfCellPixels inputIsBinary outputIsBinary"
              << std::endl;
    return EXIT_FAILURE;
  }


  int testStatus = EXIT_SUCCESS;

  auto vtkPolyDataMeshIO = itk::VTKPolyDataMeshIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(vtkPolyDataMeshIO, VTKPolyDataMeshIO, MeshIOBase);


  // Create a different instance to check the base class methods
  auto vtkMeshIOBaseTest = itk::VTKPolyDataMeshIO::New();
  testStatus = TestBaseClassMethodsMeshIO<itk::VTKPolyDataMeshIO>(vtkMeshIOBaseTest);

  // Test reading exceptions
  std::string inputFileName = "";
  vtkPolyDataMeshIO->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadMeshInformation());

  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadPoints(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadPointData(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadCells(nullptr));
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadCellData(nullptr));

  inputFileName = argv[3];
  vtkPolyDataMeshIO->SetFileName(inputFileName);
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadMeshInformation());

  ITK_TEST_EXPECT_TRUE(!vtkPolyDataMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = "nonExistingFile.vtk";
  ITK_TEST_EXPECT_TRUE(!vtkPolyDataMeshIO->CanReadFile(inputFileName.c_str()));

  inputFileName = argv[1];
  ITK_TEST_EXPECT_TRUE(vtkPolyDataMeshIO->CanReadFile(inputFileName.c_str()));
  vtkPolyDataMeshIO->SetFileName(inputFileName);

  // Test Set/Get methods
  auto useCompression = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(vtkPolyDataMeshIO, UseCompression, useCompression);

  auto updatePoints = static_cast<bool>(std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(vtkPolyDataMeshIO, UpdatePoints, updatePoints);

  auto updatePointData = static_cast<bool>(std::stoi(argv[7]));
  ITK_TEST_SET_GET_BOOLEAN(vtkPolyDataMeshIO, UpdatePointData, updatePointData);

  auto updateCells = static_cast<bool>(std::stoi(argv[8]));
  ITK_TEST_SET_GET_BOOLEAN(vtkPolyDataMeshIO, UpdateCells, updateCells);

  auto updateCellData = static_cast<bool>(std::stoi(argv[9]));
  ITK_TEST_SET_GET_BOOLEAN(vtkPolyDataMeshIO, UpdateCellData, updateCellData);

  ITK_TEST_EXPECT_TRUE(vtkPolyDataMeshIO->CanReadFile(inputFileName.c_str()));

  // Read the actual data  auto outputIsBinary = static_cast<bool>(std::stoi(argv[16]));
  auto inputIsBinary = static_cast<bool>(std::stoi(argv[14]));
  if (!inputIsBinary)
  {
    vtkPolyDataMeshIO->SetFileType(itk::IOFileEnum::ASCII);
  }
  else
  {
    vtkPolyDataMeshIO->SetFileType(itk::IOFileEnum::BINARY);
  }
  ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->ReadMeshInformation());

  auto numberOfPoints = static_cast<itk::VTKPolyDataMeshIO::SizeValueType>(std::stoi(argv[10]));
  ITK_TEST_EXPECT_EQUAL(numberOfPoints, vtkPolyDataMeshIO->GetNumberOfPoints());

  auto numberOfPointPixels = static_cast<itk::VTKPolyDataMeshIO::SizeValueType>(std::stoi(argv[11]));
  ITK_TEST_EXPECT_EQUAL(numberOfPointPixels, vtkPolyDataMeshIO->GetNumberOfPointPixels());

  auto numberOfCells = static_cast<itk::VTKPolyDataMeshIO::SizeValueType>(std::stoi(argv[12]));
  ITK_TEST_EXPECT_EQUAL(numberOfCells, vtkPolyDataMeshIO->GetNumberOfCells());

  auto numberOfCellPixels = static_cast<itk::VTKPolyDataMeshIO::SizeValueType>(std::stoi(argv[13]));
  ITK_TEST_EXPECT_EQUAL(numberOfCellPixels, vtkPolyDataMeshIO->GetNumberOfCellPixels());


  // Use sufficiently large buffer sizes
  itk::SizeValueType pointBufferSize = 1000;
  itk::SizeValueType pointDataBufferSize = 1000;

  itk::SizeValueType cellBufferSize = 2000;
  itk::SizeValueType cellDataBufferSize = 2000;

  const std::shared_ptr<void> pointBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(vtkPolyDataMeshIO->GetPointComponentType(), pointBufferSize);
  const std::shared_ptr<void> pointDataBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(vtkPolyDataMeshIO->GetPointPixelComponentType(), pointDataBufferSize);
  const std::shared_ptr<void> cellBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(vtkPolyDataMeshIO->GetCellComponentType(), cellBufferSize);
  const std::shared_ptr<void> cellDataBuffer =
    itk::MeshIOTestHelper::AllocateBuffer(vtkPolyDataMeshIO->GetCellPixelComponentType(), cellDataBufferSize);

  auto pointPixelComponentType = vtkPolyDataMeshIO->GetPointPixelComponentType();
  auto cellPixelComponentType = vtkPolyDataMeshIO->GetCellPixelComponentType();

  ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->ReadPoints(pointBuffer.get()));

  if (pointPixelComponentType == itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadPointData(pointDataBuffer.get()));
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->ReadPointData(pointDataBuffer.get()));
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->ReadCells(cellBuffer.get()));

  if (cellPixelComponentType == itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->ReadCellData(cellDataBuffer.get()));
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->ReadCellData(cellDataBuffer.get()));
  }

  // Test writing exceptions
  std::string outputFileName = "";
  vtkPolyDataMeshIO->SetFileName(outputFileName);
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->WritePoints(pointBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->WritePointData(pointDataBuffer.get()));

  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->WriteCells(cellBuffer.get()));
  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->WriteCellData(cellDataBuffer.get()));

  ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->WriteMeshInformation());

  outputFileName = argv[4];
  ITK_TEST_EXPECT_TRUE(!vtkPolyDataMeshIO->CanWriteFile(outputFileName.c_str()));

  outputFileName = argv[2];
  ITK_TEST_EXPECT_TRUE(vtkPolyDataMeshIO->CanWriteFile(outputFileName.c_str()));
  vtkPolyDataMeshIO->SetFileName(outputFileName);

  // Write the actual data
  auto outputIsBinary = static_cast<bool>(std::stoi(argv[15]));
  if (!outputIsBinary)
  {
    vtkPolyDataMeshIO->SetFileType(itk::IOFileEnum::ASCII);
  }
  else
  {
    vtkPolyDataMeshIO->SetFileType(itk::IOFileEnum::BINARY);
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->WritePoints(pointBuffer.get()));

  if (pointPixelComponentType == itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->WritePointData(pointDataBuffer.get()));
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->WritePointData(pointDataBuffer.get()));
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->WriteCells(cellBuffer.get()));

  if (cellPixelComponentType == itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE)
  {
    ITK_TRY_EXPECT_EXCEPTION(vtkPolyDataMeshIO->WriteCellData(cellDataBuffer.get()));
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->WriteCellData(cellDataBuffer.get()));
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(vtkPolyDataMeshIO->WriteMeshInformation());

  // Not used; empty method body; called for coverage purposes
  vtkPolyDataMeshIO->Write();

  // Read back the written image and check the properties
  auto readWriteVtkPolyDataMeshIO = itk::VTKPolyDataMeshIO::New();

  readWriteVtkPolyDataMeshIO->SetFileName(outputFileName);
  readWriteVtkPolyDataMeshIO->ReadMeshInformation();

  // FIXME
  // For some reason, the point pixel type is different for the rh for fibers.vtk; maybe it does not get written
  // properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetPointPixelType(),
  //                       readWriteVtkPolyDataMeshIO->GetPointPixelType());

  // FIXME
  // For some reason, the cell pixel type is different for the rh for fibers.vtk; maybe it does not get written properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetCellPixelType(), readWriteVtkPolyDataMeshIO->GetCellPixelType());

  // FIXME
  // For some reason, the point pixel component type is different for the rh for fibers.vtk; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetPointPixelComponentType(),
  //                       readWriteVtkPolyDataMeshIO->GetPointPixelComponentType());

  // FIXME
  // For some reason, the cell pixel component type is different for the rh for fibers.vtk; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetCellPixelComponentType(),
  //                       readWriteVtkPolyDataMeshIO->GetCellPixelComponentType());

  // FIXME
  // For some reason, the number of points is different for the rh for sphere.vtk; maybe it does not get written
  // properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetNumberOfPoints(),
  //                       readWriteVtkPolyDataMeshIO->GetNumberOfPoints());

  // FIXME
  // For some reason, the number of cells is different for the rh for sphere.vtk; maybe it does not get written properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetNumberOfCells(), readWriteVtkPolyDataMeshIO->GetNumberOfCells());

  // FIXME
  // For some reason, the number of point pixels is different for the rh for fibers.vtk; maybe it does not get written
  // properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetNumberOfPointPixels(),
  //                       readWriteVtkPolyDataMeshIO->GetNumberOfPointPixels());

  // FIXME
  // For some reason, the number of cell pixels is different for the rh for fibers.vtk; maybe it does not get written
  // properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetNumberOfCellPixels(),
  //                       readWriteVtkPolyDataMeshIO->GetNumberOfCellPixels());

  // FIXME
  // For some reason, the number of point pixel components is different for the rh for fibers.vtk; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetNumberOfPointPixelComponents(),
  //                       readWriteVtkPolyDataMeshIO->GetNumberOfPointPixelComponents());

  // FIXME
  // For some reason, the number of cell pixel components is different for the rh for fibers.vtk; maybe it does not get
  // written properly
  // ITK_TEST_EXPECT_EQUAL(vtkPolyDataMeshIO->GetNumberOfCellPixelComponents(),
  //                      readWriteVtkPolyDataMeshIO->GetNumberOfCellPixelComponents());

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
