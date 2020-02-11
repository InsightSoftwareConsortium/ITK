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

#include "itkOtsuMultipleThresholdsCalculator.h"

int
itkOtsuMultipleThresholdsCalculatorTest2(int, char *[])
{
  using MeasurementType = float;
  using HistogramType = itk::Statistics::Histogram<MeasurementType>;
  HistogramType::Pointer histogram = HistogramType::New();

  bool               passed = true;
  constexpr unsigned pixelCount = 4;
  for (unsigned thresholdCount = 1; thresholdCount < pixelCount; ++thresholdCount)
  {
    // initialize histogram
    HistogramType::SizeType              size;
    HistogramType::MeasurementVectorType lowerBound;
    HistogramType::MeasurementVectorType upperBound;
    lowerBound.SetSize(1);
    upperBound.SetSize(1);
    histogram->SetMeasurementVectorSize(1);
    size.SetSize(1);
    lowerBound[0] = 1000.0;
    upperBound[0] = 4000.0;
    size.Fill(pixelCount);

    histogram->Initialize(size, lowerBound, upperBound);

    if (thresholdCount == 1)
    {
      std::cout << "Bin centers:";
      for (HistogramType::Iterator iter = histogram->Begin(); iter != histogram->End(); ++iter)
      {
        iter.SetFrequency(1);
        std::cout << " " << iter.GetMeasurementVector()[0];
      }
      std::cout << std::endl;
    }

    using OtsuMultipleThresholdCalculatorType = itk::OtsuMultipleThresholdsCalculator<HistogramType>;
    OtsuMultipleThresholdCalculatorType::Pointer otsuThresholdCalculator = OtsuMultipleThresholdCalculatorType::New();

    otsuThresholdCalculator->SetInputHistogram(histogram);
    otsuThresholdCalculator->SetNumberOfThresholds(thresholdCount);

    OtsuMultipleThresholdCalculatorType::OutputType thMax;
    OtsuMultipleThresholdCalculatorType::OutputType thMid;
    try
    {
      otsuThresholdCalculator->SetReturnBinMidpoint(false);
      otsuThresholdCalculator->Compute();
      thMax = otsuThresholdCalculator->GetOutput();

      otsuThresholdCalculator->SetReturnBinMidpoint(true);
      otsuThresholdCalculator->Compute();
      thMid = otsuThresholdCalculator->GetOutput();
    }
    catch (const itk::ExceptionObject & excp)
    {
      std::cerr << excp << std::endl;
    }

    std::cout << thresholdCount << " thresholds:";
    for (unsigned long j = 0; j < thresholdCount; ++j)
    {
      std::cout << " " << thMid[j] << "(" << thMax[j] << ")";
      if (thMax[j] <= thMid[j])
      {
        passed = false;
        std::cout << "*";
      }
    }
    std::cout << std::endl;
  }

  if (!passed)
  {
    std::cout << "Test failed. Problematic thresholds marked with *" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
