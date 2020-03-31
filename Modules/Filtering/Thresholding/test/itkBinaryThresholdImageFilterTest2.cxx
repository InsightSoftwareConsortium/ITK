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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkStatisticsImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkTestingMacros.h"


int
itkBinaryThresholdImageFilterTest2(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage1"
              << " inputImage2"
              << " outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  // Threshold one image based on the statistics of another image
  //

  // Define the dimension of the images
  constexpr unsigned int Dimension = 2;

  // Declare the types of the images
  using InputPixelType = double;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());


  // Declare the filter types
  using StatisticsType = itk::StatisticsImageFilter<InputImageType>;
  using ThresholdType = itk::BinaryThresholdImageFilter<InputImageType, OutputImageType>;

  // Create the filters
  StatisticsType::Pointer statistics = StatisticsType::New();
  ThresholdType::Pointer  threshold = ThresholdType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(threshold, BinaryThresholdImageFilter, UnaryFunctorImageFilter);


  // Set up the standard pipeline connections
  statistics->SetInput(reader2->GetOutput());
  threshold->SetInput(reader->GetOutput());

  threshold->SetUpperThresholdInput(statistics->GetMeanOutput());
  ITK_TEST_SET_GET_VALUE(statistics->GetMeanOutput(), threshold->GetUpperThresholdInput());

  threshold->SetLowerThresholdInput(statistics->GetMinimumOutput());
  ITK_TEST_SET_GET_VALUE(statistics->GetMinimumOutput(), threshold->GetLowerThresholdInput());


  // Write the output
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(threshold->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
