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

#ifndef itkYenThresholdCalculator_hxx
#define itkYenThresholdCalculator_hxx

#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template <typename THistogram, typename TOutput>
void
YenThresholdCalculator<THistogram, TOutput>::GenerateData()
{
  const HistogramType * histogram = this->GetInput();

  if (histogram->GetTotalFrequency() == 0)
  {
    itkExceptionMacro("Histogram is empty");
  }
  const ProgressReporter progress(this, 0, histogram->GetSize(0));
  if (histogram->GetSize(0) == 1)
  {
    this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(0, 0)));
  }

  const unsigned int size = histogram->GetSize(0);

  typename HistogramType::InstanceIdentifier threshold = 0;

  std::vector<double> norm_histo(size); // normalized histogram
  const int           total = histogram->GetTotalFrequency();

  for (int ih = 0; static_cast<unsigned int>(ih) < size; ++ih)
  {
    norm_histo[ih] = static_cast<double>(histogram->GetFrequency(ih, 0)) / total;
  }

  std::vector<double> P1(size); // cumulative normalized histogram
  P1[0] = norm_histo[0];
  for (int ih = 1; static_cast<unsigned int>(ih) < size; ++ih)
  {
    P1[ih] = P1[ih - 1] + norm_histo[ih];
  }
  std::vector<double> P1_sq(size);
  P1_sq[0] = norm_histo[0] * norm_histo[0];
  for (int ih = 1; static_cast<unsigned int>(ih) < size; ++ih)
  {
    P1_sq[ih] = P1_sq[ih - 1] + norm_histo[ih] * norm_histo[ih];
  }
  std::vector<double> P2_sq(size);
  P2_sq[size - 1] = 0.0;
  for (int ih = static_cast<unsigned int>(size) - 2; ih >= 0; ih--)
  {
    P2_sq[ih] = P2_sq[ih + 1] + norm_histo[ih + 1] * norm_histo[ih + 1];
  }

  // Find the threshold that maximizes the criterion
  double max_crit = itk::NumericTraits<double>::NonpositiveMin();
  for (int it = 0; static_cast<unsigned int>(it) < size; ++it)
  {
    const double crit = -1.0 * ((P1_sq[it] * P2_sq[it]) > 0.0 ? std::log(P1_sq[it] * P2_sq[it]) : 0.0) +
                        2 * ((P1[it] * (1.0 - P1[it])) > 0.0 ? std::log(P1[it] * (1.0 - P1[it])) : 0.0);
    if (crit > max_crit)
    {
      max_crit = crit;
      threshold = it;
    }
  }

  this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(threshold, 0)));
}

} // end namespace itk

#endif
