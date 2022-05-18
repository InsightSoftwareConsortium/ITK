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

#include "itkIsoDataThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkIsoDataThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cout << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " numberOfHistogramBins"
              << " autoMinimumMaximum"
              << " expectedThreshold" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using FilterType = itk::IsoDataThresholdImageFilter<InputImageType, OutputImageType>;
  auto filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, IsoDataThresholdImageFilter, HistogramThresholdImageFilter);


  auto insideValue = static_cast<FilterType::OutputPixelType>(255);
  filter->SetInsideValue(insideValue);
  ITK_TEST_SET_GET_VALUE(insideValue, filter->GetInsideValue());

  auto outsideValue = static_cast<FilterType::OutputPixelType>(0);
  filter->SetOutsideValue(outsideValue);
  ITK_TEST_SET_GET_VALUE(outsideValue, filter->GetOutsideValue());

  auto numberOfHistogramBins = static_cast<itk::SizeValueType>(std::stoi(argv[3]));
  filter->SetNumberOfHistogramBins(numberOfHistogramBins);
  ITK_TEST_SET_GET_VALUE(numberOfHistogramBins, filter->GetNumberOfHistogramBins());

  auto autoMinimumMaximum = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(filter, AutoMinimumMaximum, autoMinimumMaximum);

  // Test no histogram exception (no input set)
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  filter->SetInput(reader->GetOutput());

  // Test no calculator set exception
  filter->SetCalculator(nullptr);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  FilterType::CalculatorType::Pointer calculator = FilterType::CalculatorType::New();
  filter->SetCalculator(calculator);
  ITK_TEST_SET_GET_VALUE(calculator, filter->GetCalculator());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  auto                       expectedThreshold = static_cast<FilterType::InputPixelType>(std::stod(argv[5]));
  FilterType::InputPixelType resultThreshold = filter->GetThreshold();
  if (itk::Math::NotAlmostEquals(expectedThreshold, resultThreshold))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetThreshold()" << std::endl;
    std::cerr << "Expected: " << itk::NumericTraits<FilterType::InputPixelType>::PrintType(expectedThreshold)
              << ", but got: " << itk::NumericTraits<FilterType::InputPixelType>::PrintType(resultThreshold)
              << std::endl;
    return EXIT_FAILURE;
  }

  // Write output image
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
