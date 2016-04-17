/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

int itkOtsuMultipleThresholdsCalculatorTest(int, char*[])
{
  typedef float                                         MeasurementType;
  typedef itk::Statistics::Histogram< MeasurementType > HistogramType;
  HistogramType::Pointer histogram = HistogramType::New();

  // initialize histogram
  HistogramType::SizeType size;
  HistogramType::MeasurementVectorType lowerBound;
  HistogramType::MeasurementVectorType upperBound;
  lowerBound.SetSize(1);
  upperBound.SetSize(1);
  histogram->SetMeasurementVectorSize(1);
  size.SetSize(1);
  lowerBound[0] = 0.0;
  upperBound[0] = 64.0;
  size.Fill(64);

  histogram->Initialize(size, lowerBound, upperBound );

  // create vector of values.
  typedef std::vector<MeasurementType> ValuesVectorType;
  ValuesVectorType values;
  values.push_back(8.0);
  values.push_back(16.0);
  values.push_back(32.0);
  values.push_back(48.0);

  MeasurementType range = 2.0;

  // create histogram with samples at values +- range.
  for (HistogramType::Iterator iter = histogram->Begin(); iter != histogram->End(); ++iter)
    {
    HistogramType::MeasurementType measurement = iter.GetMeasurementVector()[0];

    for (ValuesVectorType::const_iterator viter = values.begin(); viter != values.end(); ++viter)
      {
      if (measurement > (*viter-range) && measurement <  (*viter+range))
        {
        iter.SetFrequency(1);
        }
      }
    }

  // Compute numberOfValues - 1 thresholds.
  size_t numberOfThresholds = values.size() - 1;

  typedef itk::OtsuMultipleThresholdsCalculator<HistogramType>  OtsuMultipleThresholdCalculatorType;

  OtsuMultipleThresholdCalculatorType::Pointer otsuThresholdCalculator = OtsuMultipleThresholdCalculatorType::New();

  otsuThresholdCalculator->SetInputHistogram(histogram.GetPointer());
  otsuThresholdCalculator->SetNumberOfThresholds(numberOfThresholds);

  try
    {
    otsuThresholdCalculator->Compute();
    }
  catch(itk::ExceptionObject & excp)
    {
    std::cerr << excp << std::endl;
    }
  otsuThresholdCalculator->Print (std::cout);

  OtsuMultipleThresholdCalculatorType::OutputType otsuThresholds = otsuThresholdCalculator->GetOutput();

  bool passed = true;

  // Check if thresholds correctly separate values.
  for (unsigned long j = 0; j<numberOfThresholds; ++j)
    {
    if (otsuThresholds[j] < values[j] || otsuThresholds[j] > values[j+1])
      {
      passed = false;
      break;
      }
    }

  if (!passed)
    {
      std::cout << "Test failed." << std::endl;
      std::cout << otsuThresholdCalculator << std::endl;
      return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
