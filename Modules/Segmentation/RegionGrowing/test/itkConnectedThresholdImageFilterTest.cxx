/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <fstream>
#include "itkConnectedThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkConnectedThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage OutputImage "
              << "seed_x seed_y "
              << "LowerConnectedThreshold UpperConnectedThreshold "
              << "Connectivity[1=Full,0=Face]" << std::endl;
    return -1;
  }


  // Define the dimension of the images
  constexpr unsigned int Dimension = 2;

  // Define the pixel types of the images
  using PixelType = unsigned char;

  // Define the types of the images
  using ImageType = itk::Image<PixelType, Dimension>;


  itk::ImageFileReader<ImageType>::Pointer imageReader = itk::ImageFileReader<ImageType>::New();

  std::string inputImageFilename = argv[1];
  imageReader->SetFileName(inputImageFilename);

  ITK_TRY_EXPECT_NO_EXCEPTION(imageReader->Update(););

  // Create the filter
  using ConnectedThresholdImageFilterType = itk::ConnectedThresholdImageFilter<ImageType, ImageType>;

  ConnectedThresholdImageFilterType::Pointer connectedThresholdFilter = ConnectedThresholdImageFilterType::New();

  itk::SimpleFilterWatcher watcher(connectedThresholdFilter);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(connectedThresholdFilter, ConnectedThresholdImageFilter, ImageToImageFilter);


  ConnectedThresholdImageFilterType::IndexType seed;
  seed[0] = std::stoi(argv[3]);
  seed[1] = std::stoi(argv[4]);

  connectedThresholdFilter->AddSeed(seed);
  ConnectedThresholdImageFilterType::SeedContainerType seedContainer = connectedThresholdFilter->GetSeeds();

  connectedThresholdFilter->ClearSeeds();
  seedContainer = connectedThresholdFilter->GetSeeds();
  if (!seedContainer.empty())
  {
    std::cerr << "Test FAILED !" << std::endl;
    std::cerr << "Seed container not empty after clearing filter seed container !" << std::endl;
    return EXIT_FAILURE;
  }

  connectedThresholdFilter->SetSeed(seed);


  ConnectedThresholdImageFilterType::InputPixelObjectType * lowerInputPixelObject =
    connectedThresholdFilter->GetLowerInput();
  connectedThresholdFilter->SetLower(lowerInputPixelObject->Get());
  ITK_TEST_SET_GET_VALUE(lowerInputPixelObject->Get(), connectedThresholdFilter->GetLower());

  ConnectedThresholdImageFilterType::InputPixelObjectType * upperInputPixelObject =
    connectedThresholdFilter->GetUpperInput();
  connectedThresholdFilter->SetUpper(upperInputPixelObject->Get());
  ITK_TEST_SET_GET_VALUE(upperInputPixelObject->Get(), connectedThresholdFilter->GetUpper());


  ConnectedThresholdImageFilterType::InputImagePixelType lowerThreshold = std::stoi(argv[5]);
  connectedThresholdFilter->SetLower(lowerThreshold);
  ITK_TEST_SET_GET_VALUE(lowerThreshold, connectedThresholdFilter->GetLower());

  ConnectedThresholdImageFilterType::InputImagePixelType upperThreshold = std::stoi(argv[6]);
  connectedThresholdFilter->SetUpper(upperThreshold);
  ITK_TEST_SET_GET_VALUE(upperThreshold, connectedThresholdFilter->GetUpper());

  ConnectedThresholdImageFilterType::OutputImagePixelType replaceValue = 255;
  connectedThresholdFilter->SetReplaceValue(replaceValue);
  ITK_TEST_SET_GET_VALUE(replaceValue, connectedThresholdFilter->GetReplaceValue());

  // Test the use of full (8 connectivity in 2D) on this image
  if (argc > 7)
  {
    ConnectedThresholdImageFilterType::ConnectivityEnum conenctivity =
      std::stoi(argv[7]) ? ConnectedThresholdImageFilterType::ConnectivityEnum::FullConnectivity
                         : ConnectedThresholdImageFilterType::ConnectivityEnum::FaceConnectivity;
    connectedThresholdFilter->SetConnectivity(conenctivity);
    ITK_TEST_SET_GET_VALUE(conenctivity, connectedThresholdFilter->GetConnectivity());
  }

  connectedThresholdFilter->SetInput(imageReader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(connectedThresholdFilter->Update(););

  // Write the output image
  itk::ImageFileWriter<ImageType>::Pointer writer = itk::ImageFileWriter<ImageType>::New();

  std::string outputImageFilename = argv[2];
  writer->SetFileName(outputImageFilename);

  writer->SetInput(connectedThresholdFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // Test streaming enumeration for ConnectedThresholdImageFilterEnums::Connectivity elements
  const std::set<itk::ConnectedThresholdImageFilterEnums::Connectivity> allConnectivity{
    itk::ConnectedThresholdImageFilterEnums::Connectivity::FaceConnectivity,
    itk::ConnectedThresholdImageFilterEnums::Connectivity::FullConnectivity
  };
  for (const auto & ee : allConnectivity)
  {
    std::cout << "STREAMED ENUM VALUE ConnectedThresholdImageFilterEnums::Connectivity: " << ee << std::endl;
  }

  return EXIT_SUCCESS;
}
