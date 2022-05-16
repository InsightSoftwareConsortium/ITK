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
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " InputImage OutputImage FindUpper(true,false) seed1_x seed1_y seed2_x seed2_y [seed1_x2 seed1_y2 "
                 "seed2_x2 seed2_y2]*\n";
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
  filter->SetLower(0);
#if !defined(ITK_LEGACY_REMOVE)
  filter->SetUpperValueLimit(255); // deprecated method
#endif
  filter->SetUpper(255);
  filter->SetReplaceValue(255);

  // Test SetMacro
  filter->SetIsolatedValueTolerance(1);

  // Test SetMacro
  std::string findUpper = argv[3];
  if (findUpper == "true")
  {
    filter->FindUpperThresholdOn();
  }
  else
  {
    filter->FindUpperThresholdOff();
  }

  // Test GetMacros
  PixelType lower = filter->GetLower();
  std::cout << "filter->GetLower(): " << static_cast<itk::NumericTraits<PixelType>::PrintType>(lower) << std::endl;
  PixelType isolatedValueTolerance = filter->GetIsolatedValueTolerance();
  std::cout << "filter->GetIsolatedValueTolerance(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(isolatedValueTolerance) << std::endl;
#if !defined(ITK_LEGACY_REMOVE)
  PixelType upperValueLimit = filter->GetUpperValueLimit();
  std::cout << "filter->GetUpperValueLimit(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(upperValueLimit) << std::endl;
#endif
  PixelType upper = filter->GetUpper();
  std::cout << "filter->GetUpper(): " << static_cast<itk::NumericTraits<PixelType>::PrintType>(upper) << std::endl;
  PixelType replaceValue = filter->GetReplaceValue();
  std::cout << "filter->GetReplaceValue(): " << static_cast<itk::NumericTraits<PixelType>::PrintType>(replaceValue)
            << std::endl;
  bool findUpperThreshold = filter->GetFindUpperThreshold();
  std::cout << "filter->GetFindUpperThreshold(): " << findUpperThreshold << std::endl;

  try
  {
    input->Update();
    filter->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
  }

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
  writer->Update();


  // Now flip the mode to test whether it fails
  if (findUpper == "true")
  {
    filter->FindUpperThresholdOff();
  }
  else
  {
    filter->FindUpperThresholdOn();
  }

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
  }

  thresholdingFailed = filter->GetThresholdingFailed();

  if (thresholdingFailed)
  {
    std::cout << "When mode flipped: Selection of isolating threshold failed" << std::endl;
  }
  else
  {
    std::cout << "When mode flipped: Selection of isolating threshold succeeded" << std::endl;
  }


  return EXIT_SUCCESS;
}
