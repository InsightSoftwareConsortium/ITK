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

#include "itkOtsuMultipleThresholdsCalculator.h"
#include "itkTestingMacros.h"


int
itkOtsuMultipleThresholdsCalculatorTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " valleyEmphasis returnBinMidpoint" << std::endl;
    return EXIT_FAILURE;
  }

  using MeasurementType = float;
  using HistogramType = itk::Statistics::Histogram<MeasurementType>;

  auto histogram = HistogramType::New();

  // Initialize histogram
  HistogramType::SizeType              size;
  HistogramType::MeasurementVectorType lowerBound;
  HistogramType::MeasurementVectorType upperBound;
  lowerBound.SetSize(1);
  upperBound.SetSize(1);
  histogram->SetMeasurementVectorSize(1);
  size.SetSize(1);
  lowerBound[0] = 0.0;
  upperBound[0] = 64.0;
  size.Fill(64);

  histogram->Initialize(size, lowerBound, upperBound);

  // Create vector of values
  using ValuesVectorType = std::vector<MeasurementType>;
  ValuesVectorType values;
  values.push_back(8.0);
  values.push_back(16.0);
  values.push_back(32.0);
  values.push_back(48.0);

  MeasurementType range = 2.0;

  // Create histogram with samples at values +- range
  for (HistogramType::Iterator iter = histogram->Begin(); iter != histogram->End(); ++iter)
  {
    HistogramType::MeasurementType measurement = iter.GetMeasurementVector()[0];

    for (ValuesVectorType::const_iterator viter = values.begin(); viter != values.end(); ++viter)
    {
      if (measurement > (*viter - range) && measurement < (*viter + range))
      {
        iter.SetFrequency(1);
      }
    }
  }

  // Compute numberOfValues - 1 thresholds
  auto numberOfThresholds = values.size() - 1;

  using OtsuMultipleThresholdCalculatorType = itk::OtsuMultipleThresholdsCalculator<HistogramType>;

  auto otsuThresholdCalculator = OtsuMultipleThresholdCalculatorType::New();

#if defined(ITKV4_COMPATIBILITY)
  ITK_TEST_EXPECT_TRUE(otsuThresholdCalculator->GetReturnBinMidpoint());
#else
  ITK_TEST_EXPECT_TRUE(!otsuThresholdCalculator->GetReturnBinMidpoint());
#endif

  otsuThresholdCalculator->SetInputHistogram(histogram);
  otsuThresholdCalculator->SetNumberOfThresholds(numberOfThresholds);

  otsuThresholdCalculator->SetInputHistogram(histogram.GetPointer());

  otsuThresholdCalculator->SetNumberOfThresholds(numberOfThresholds);
  ITK_TEST_SET_GET_VALUE(numberOfThresholds, otsuThresholdCalculator->GetNumberOfThresholds());

  bool valleyEmphasis = std::stoi(argv[1]);
  ITK_TEST_SET_GET_BOOLEAN(otsuThresholdCalculator, ValleyEmphasis, valleyEmphasis);

  bool returnBinMidpoint = std::stoi(argv[2]);
  ITK_TEST_SET_GET_BOOLEAN(otsuThresholdCalculator, ReturnBinMidpoint, returnBinMidpoint);

  ITK_TRY_EXPECT_NO_EXCEPTION(otsuThresholdCalculator->Compute());


  OtsuMultipleThresholdCalculatorType::OutputType otsuThresholds = otsuThresholdCalculator->GetOutput();

  // Check if thresholds correctly separate values
  for (unsigned long j = 0; j < numberOfThresholds; ++j)
  {
    if (otsuThresholds[j] < values[j] || otsuThresholds[j] > values[j + 1])
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in GetOutput() at threshold: " << j << std::endl;
      std::cerr << "Expected value to be between: " << values[j] << " and " << values[j + 1]
                << ", but got: " << otsuThresholds[j] << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
