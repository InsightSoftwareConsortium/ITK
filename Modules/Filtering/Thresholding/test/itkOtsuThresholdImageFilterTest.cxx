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

#include "itkOtsuThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkTestingMacros.h"


int
itkOtsuThresholdImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " numberOfHistogramBins"
              << " autoMinimumMaximum"
              << " expectedThreshold"
              << " [flipOutputIntensities]"
              << " [returnBinMidpoint]" << std::endl;
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


  using FilterType = itk::OtsuThresholdImageFilter<InputImageType, OutputImageType>;
  FilterType::Pointer filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, OtsuThresholdImageFilter, HistogramThresholdImageFilter);


#if defined(ITKV4_COMPATIBILITY)
  ITK_TEST_EXPECT_TRUE(filter->GetReturnBinMidpoint());
#else
  ITK_TEST_EXPECT_TRUE(!filter->GetReturnBinMidpoint());
#endif


  auto numberOfHistogramBins = static_cast<itk::SizeValueType>(std::stoi(argv[3]));
  filter->SetNumberOfHistogramBins(numberOfHistogramBins);
  ITK_TEST_SET_GET_VALUE(numberOfHistogramBins, filter->GetNumberOfHistogramBins());

  auto autoMinimumMaximum = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(filter, AutoMinimumMaximum, autoMinimumMaximum);

  // Test no histogram exception (no input set)
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  FilterType::CalculatorType::Pointer calculator = FilterType::CalculatorType::New();
  filter->SetCalculator(calculator);
  ITK_TEST_SET_GET_VALUE(calculator, filter->GetCalculator());


  filter->SetInput(reader->GetOutput());

  if (argc > 5)
  {
    bool flipOutputIntensities = std::stoi(argv[6]);
    if (flipOutputIntensities)
    {
      // Flip the inside and outside values
      FilterType::OutputPixelType outsideValue = filter->GetInsideValue();
      FilterType::OutputPixelType insideValue = filter->GetOutsideValue();
      filter->SetInsideValue(insideValue);
      filter->SetOutsideValue(outsideValue);
    }
  }
  if (argc > 6)
  {
    bool returnBinMidpoint = static_cast<bool>(std::stoi(argv[6]));
    ITK_TEST_SET_GET_BOOLEAN(filter, ReturnBinMidpoint, returnBinMidpoint);
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Regression test: compare computed threshold
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
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
