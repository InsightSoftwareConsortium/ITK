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

#include <iostream>
#include "itkDerivativeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkDerivativeImageFilterTest(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile normalizedOutputImageFile "
              << " derivativeOrder direction useImageSpacing" << std::endl;
    return EXIT_FAILURE;
  }


  // Test using an unsigned integral pixel type and generate a signed
  // integral pixel type
  using InputPixelType = unsigned short;
  using OutputPixelType = short;

  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Define the filter
  using FilterType = itk::DerivativeImageFilter<InputImageType, OutputImageType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, DerivativeImageFilter, ImageToImageFilter);

  // Set up the filter
  unsigned int order = std::stoi(argv[3]);
  filter->SetOrder(order);
  ITK_TEST_SET_GET_VALUE(order, filter->GetOrder());

  unsigned int direction = std::stoi(argv[4]);
  filter->SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, filter->GetDirection());

  auto useImageSpacing = static_cast<bool>(std::stoi(argv[5]));
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (useImageSpacing)
  {
    filter->SetUseImageSpacingOn();
  }
  else
  {
    filter->SetUseImageSpacingOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);


  itk::SimpleFilterWatcher watcher(filter, "Derivative");

  // wire the pipeline
  filter->SetInput(reader->GetOutput());

  // Write the output
  using WriteImageType = itk::Image<unsigned char, Dimension>;

  using NormalizeFilterType = itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>;

  using NormalizedWriterType = itk::ImageFileWriter<WriteImageType>;

  auto normalizer = NormalizeFilterType::New();
  auto normalizedWriter = NormalizedWriterType::New();

  normalizer->SetInput(filter->GetOutput());
  normalizedWriter->SetInput(normalizer->GetOutput());

  normalizer->SetOutputMinimum(0);
  normalizer->SetOutputMaximum(255);

  normalizedWriter->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(normalizedWriter->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
