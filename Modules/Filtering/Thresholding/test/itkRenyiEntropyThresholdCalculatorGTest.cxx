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

#include "itkRenyiEntropyThresholdCalculator.h"
#include "itkHistogram.h"

#include "itkGTest.h"

namespace itk
{

template <typename THistogram, typename TOutput>
class RenyiEntropyThresholdCalculatorGTestHelper : public RenyiEntropyThresholdCalculator<THistogram, TOutput>
{
public:
  using Self = RenyiEntropyThresholdCalculatorGTestHelper;
  using Superclass = RenyiEntropyThresholdCalculator<THistogram, TOutput>;
  using Pointer = SmartPointer<Self>;

  itkNewMacro(Self);

  using typename Superclass::HistogramType;
  using typename Superclass::InstanceIdentifier;
  using Superclass::MaxEntropyThresholding;
  using Superclass::MaxEntropyThresholding2;
};

} // namespace itk

namespace
{

// Two-bin histogram where every candidate threshold ties for maximum entropy.
template <typename FilterType>
typename FilterType::HistogramType::Pointer
MakeTwoSpikeHistogram()
{
  using HistogramType = typename FilterType::HistogramType;
  auto histogram = HistogramType::New();
  histogram->SetMeasurementVectorSize(1);

  typename HistogramType::SizeType size(1);
  size.Fill(5);
  typename HistogramType::MeasurementVectorType lowerBound(1);
  typename HistogramType::MeasurementVectorType upperBound(1);
  lowerBound[0] = 0.0;
  upperBound[0] = 5.0;
  histogram->Initialize(size, lowerBound, upperBound);
  histogram->SetFrequency(1, 1);
  histogram->SetFrequency(3, 1);

  return histogram;
}

} // namespace

TEST(RenyiEntropyThresholdCalculator, MaxEntropyThresholdingBreaksTieWithNonpositiveMinSeed)
{
  using HistogramType = itk::Statistics::Histogram<double>;
  using FilterType = itk::RenyiEntropyThresholdCalculatorGTestHelper<HistogramType, double>;
  using InstanceIdentifier = FilterType::InstanceIdentifier;

  auto histogram = MakeTwoSpikeHistogram<FilterType>();

  auto filter = FilterType::New();
  filter->SetInput(histogram);
  filter->Update(); // Sets the private m_FirstBin/m_LastBin/m_Size to match this histogram.

  std::vector<double> normHisto(5);
  std::vector<double> P1(5);
  std::vector<double> P2(5);
  const double        total = histogram->GetTotalFrequency();
  for (unsigned int i = 0; i < 5; ++i)
  {
    normHisto[i] = histogram->GetFrequency(i, 0) / total;
  }
  P1[0] = normHisto[0];
  P2[0] = 1.0 - P1[0];
  for (unsigned int i = 1; i < 5; ++i)
  {
    P1[i] = P1[i - 1] + normHisto[i];
    P2[i] = 1.0 - P1[i];
  }

  const InstanceIdentifier threshold1 = filter->MaxEntropyThresholding(histogram, normHisto, P1, P2);
  const InstanceIdentifier threshold2 = filter->MaxEntropyThresholding2(histogram, normHisto, P1, P2);

  // A NonpositiveMin()-seeded max must accept the first tie (bin 1), not fall through to bin 0.
  EXPECT_EQ(threshold1, InstanceIdentifier{ 1 });
  EXPECT_EQ(threshold2, InstanceIdentifier{ 1 });
}
