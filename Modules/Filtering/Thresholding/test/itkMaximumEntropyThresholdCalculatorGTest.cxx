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

#include "itkMaximumEntropyThresholdCalculator.h"
#include "itkHistogram.h"

#include "itkGTest.h"

TEST(MaximumEntropyThresholdCalculator, BreaksEntropyTieWithNonpositiveMinSeed)
{
  using HistogramType = itk::Statistics::Histogram<double>;
  using CalculatorType = itk::MaximumEntropyThresholdCalculator<HistogramType, double>;

  // Two-bin histogram where every candidate threshold ties for maximum entropy.
  auto histogram = HistogramType::New();
  histogram->SetMeasurementVectorSize(1);

  HistogramType::SizeType size(1);
  size.Fill(5);
  HistogramType::MeasurementVectorType lowerBound(1);
  HistogramType::MeasurementVectorType upperBound(1);
  lowerBound[0] = 0.0;
  upperBound[0] = 5.0;
  histogram->Initialize(size, lowerBound, upperBound);
  histogram->SetFrequency(1, 1);
  histogram->SetFrequency(3, 1);

  auto calculator = CalculatorType::New();
  calculator->SetInput(histogram);
  calculator->Update();

  // A NonpositiveMin()-seeded max must accept the first tie (bin 1), not fall through to bin 0.
  EXPECT_DOUBLE_EQ(calculator->GetThreshold(), histogram->GetMeasurement(1, 0));
}
