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
#include "itkImageFileReader.h"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkTestingMacros.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkMeshFileReader.h"
#include <iostream>
#include <string>

int
itkConnectedRegionsMeshFilterTest2(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " Input NumberOfConnectedComponents NumberOfCellsInLargestComponent" << std::endl;
    return EXIT_FAILURE;
  }

  // Check if input file is a mesh file or image file by checking presence of .vtk
  std::string fileName(argv[1]);
  size_t      found = fileName.find(".vtk");
  bool        imageSource = true;

  if (found != std::string::npos)
  {
    imageSource = false;
  }

  const unsigned int Dimension = 3;
  using MeshType = itk::Mesh<float, Dimension>;
  MeshType::Pointer mesh;

  if (imageSource)
  {
    // Read 3D Image to create test mesh
    using PixelType = unsigned char;
    using ImageType = itk::Image<PixelType, Dimension>;

    using ReaderType = itk::ImageFileReader<ImageType>;
    auto reader = ReaderType::New();
    reader->SetFileName(fileName);
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    auto inputImage = reader->GetOutput();

    // Threshold the 3D image to get binary mask for mesh generation
    using ThresholdFilterType = itk::BinaryThresholdImageFilter<ImageType, ImageType>;
    auto thresholdFilter = ThresholdFilterType::New();
    thresholdFilter->SetInput(inputImage);
    thresholdFilter->SetLowerThreshold(0);
    thresholdFilter->SetUpperThreshold(200);
    thresholdFilter->SetOutsideValue(1);
    thresholdFilter->SetInsideValue(0);
    ITK_TRY_EXPECT_NO_EXCEPTION(thresholdFilter->Update());

    auto outputImage = thresholdFilter->GetOutput();

    // Get mesh from binary image
    using MeshSourceType = itk::BinaryMask3DMeshSource<ImageType, MeshType>;

    auto meshSource = MeshSourceType::New();
    meshSource->SetInput(outputImage);
    meshSource->SetObjectValue(1);
    ITK_TRY_EXPECT_NO_EXCEPTION(meshSource->Update());
    mesh = meshSource->GetOutput();
  }
  else
  {
    // Read the test mesh using MeshFileReader
    using ReaderType = itk::MeshFileReader<MeshType>;
    ReaderType::Pointer polyDataReader = ReaderType::New();
    polyDataReader->SetFileName(fileName);
    ITK_TRY_EXPECT_NO_EXCEPTION(polyDataReader->Update());
    mesh = polyDataReader->GetOutput();
  }

  unsigned int numberOfConnectedComponents = std::stoi(argv[2]);
  unsigned int numberOfCellsInLargestComponent = std::stoi(argv[3]);

  // Check number of connected components in the mesh
  using ConnectFilterType = itk::ConnectedRegionsMeshFilter<MeshType, MeshType>;
  auto connectivityFilter = ConnectFilterType::New();
  connectivityFilter->SetInput(mesh);
  connectivityFilter->SetExtractionModeToAllRegions();
  ITK_TRY_EXPECT_NO_EXCEPTION(connectivityFilter->Update());
  ITK_TEST_EXPECT_TRUE(connectivityFilter->GetNumberOfExtractedRegions() == numberOfConnectedComponents);

  // Check the number of cells in the largest connected component
  connectivityFilter->SetExtractionModeToLargestRegion();
  ITK_TRY_EXPECT_NO_EXCEPTION(connectivityFilter->Update());
  ITK_TEST_EXPECT_TRUE(connectivityFilter->GetOutput()->GetNumberOfCells() == numberOfCellsInLargestComponent);

  return EXIT_SUCCESS;
}
