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

#include "itkIntermodesThresholdImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkIntermodesThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc != 8)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " numberOfHistogramBins"
              << " autoMinimumMaximum"
              << " maximumSmoothingIterations"
              << " useInterMode"
              << " expectedThreshold" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using FilterType = itk::IntermodesThresholdImageFilter<InputImageType, OutputImageType>;
  FilterType::Pointer filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, IntermodesThresholdImageFilter, HistogramThresholdImageFilter);


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
  filter->SetCalculator(0);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  FilterType::CalculatorType::Pointer calculator = FilterType::CalculatorType::New();
  filter->SetCalculator(calculator);
  ITK_TEST_SET_GET_VALUE(calculator, filter->GetCalculator());


  // Test exceeding maximum iterations for histogram smoothing exception
  filter->SetMaximumSmoothingIterations(10);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());

  unsigned long maximumSmoothingIterations = std::stoi(argv[5]);
  filter->SetMaximumSmoothingIterations(maximumSmoothingIterations);
  ITK_TEST_SET_GET_VALUE(maximumSmoothingIterations, filter->GetMaximumSmoothingIterations());

  bool useInterMode = static_cast<bool>(std::stoi(argv[6]));
  filter->SetUseInterMode(useInterMode);
  ITK_TEST_SET_GET_VALUE(useInterMode, filter->GetUseInterMode());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Regression test: compare computed threshold
  FilterType::InputPixelType expectedThreshold = std::stod(argv[7]);
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
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
