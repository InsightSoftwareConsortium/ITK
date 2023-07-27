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

#include <fstream>
#include "itkIsolatedConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkIsolatedConnectedImageFilterTest(int argc, char * argv[])
{
  if (argc < 8)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " InputImage OutputImage FindUpper seed1_x seed1_y seed2_x seed2_y [seed1_x2 seed1_y2"
                 " seed2_x2 seed2_y2]"
              << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  using myImage = itk::Image<PixelType, 2>;
  itk::ImageFileReader<myImage>::Pointer input = itk::ImageFileReader<myImage>::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::IsolatedConnectedImageFilter<myImage, myImage>;

  auto                     filter = FilterType::New();
  itk::SimpleFilterWatcher watcher(filter);

  filter->SetInput(input->GetOutput());

  FilterType::IndexType seed1;

#if !defined(ITK_LEGACY_REMOVE)
  seed1[0] = std::stoi(argv[4]);
  seed1[1] = std::stoi(argv[5]);
  filter->SetSeed1(seed1); // deprecated method

  seed1[0] = std::stoi(argv[6]);
  seed1[1] = std::stoi(argv[7]);
  filter->SetSeed2(seed1); // deprecated method
#endif

  // Clear the seeds and then add all of the seeds
  filter->ClearSeeds1();
  filter->ClearSeeds2();
  for (int i = 4; i < argc; i += 4)
  {
    seed1[0] = std::stoi(argv[i]);
    seed1[1] = std::stoi(argv[i + 1]);
    filter->AddSeed1(seed1);

    seed1[0] = std::stoi(argv[i + 2]);
    seed1[1] = std::stoi(argv[i + 3]);
    filter->AddSeed2(seed1);
  }

  // The min and max values for a .png image
  FilterType::InputImagePixelType lower = 0;
  filter->SetLower(lower);
  ITK_TEST_SET_GET_VALUE(lower, filter->GetLower());

#if !defined(ITK_LEGACY_REMOVE)
  FilterType::InputImagePixelType upperValueLimit = 255;
  filter->SetUpperValueLimit(upperValueLimit);
  ITK_TEST_SET_GET_VALUE(upperValueLimit, filter->GetUpperValueLimit());
#endif
  FilterType::InputImagePixelType upper = 255;
  filter->SetUpper(upper);
  ITK_TEST_SET_GET_VALUE(upper, filter->GetUpper());

  FilterType::OutputImagePixelType replaceValue = 255;
  filter->SetReplaceValue(replaceValue);
  ITK_TEST_SET_GET_VALUE(replaceValue, filter->GetReplaceValue());

  FilterType::InputImagePixelType isolatedValueTolerance = 1;
  filter->SetIsolatedValueTolerance(isolatedValueTolerance);
  ITK_TEST_SET_GET_VALUE(isolatedValueTolerance, filter->GetIsolatedValueTolerance());

  auto findUpperThreshold = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(filter, FindUpperThreshold, findUpperThreshold);

  ITK_TRY_EXPECT_NO_EXCEPTION(input->Update());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  bool thresholdingFailed = filter->GetThresholdingFailed();

  if (thresholdingFailed)
  {
    std::cout << "Selection of isolating threshold failed" << std::endl;
  }
  else
  {
    std::cout << "Selection of isolating threshold succeeded" << std::endl;
  }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Now flip the mode to test whether it fails
  ITK_TEST_SET_GET_BOOLEAN(filter, FindUpperThreshold, !findUpperThreshold);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  thresholdingFailed = filter->GetThresholdingFailed();

  if (thresholdingFailed)
  {
    std::cout << "When mode flipped: Selection of isolating threshold failed" << std::endl;
  }
  else
  {
    std::cout << "When mode flipped: Selection of isolating threshold succeeded" << std::endl;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
