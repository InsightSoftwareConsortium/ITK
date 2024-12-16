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

#ifndef itkShanbhagThresholdCalculator_hxx
#define itkShanbhagThresholdCalculator_hxx

#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template <typename THistogram, typename TOutput>
void
ShanbhagThresholdCalculator<THistogram, TOutput>::GenerateData()
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

  constexpr double                           tolerance = 2.220446049250313E-16;
  typename HistogramType::InstanceIdentifier threshold = 0;

  std::vector<double> norm_histo(size); // normalized histogram

  const int total = histogram->GetTotalFrequency();

  for (int ih = 0; static_cast<unsigned int>(ih) < size; ++ih)
  {
    norm_histo[ih] = static_cast<double>(histogram->GetFrequency(ih, 0)) / total;
  }
  std::vector<double> P1(size); // cumulative normalized histogram
  P1[0] = norm_histo[0];
  std::vector<double> P2(size);
  P2[0] = 1.0 - P1[0];
  for (int ih = 1; static_cast<unsigned int>(ih) < size; ++ih)
  {
    P1[ih] = P1[ih - 1] + norm_histo[ih];
    P2[ih] = 1.0 - P1[ih];
  }
  // Determine the first non-zero bin
  int first_bin = 0;
  for (int ih = 0; static_cast<unsigned int>(ih) < size; ++ih)
  {
    if (!(itk::Math::abs(P1[ih]) < tolerance))
    {
      first_bin = ih;
      break;
    }
  }

  // Determine the last non-zero bin
  int last_bin = size - 1;
  for (int ih = size - 1; ih >= first_bin; ih--)
  {
    if (!(itk::Math::abs(P2[ih]) < tolerance))
    {
      last_bin = ih;
      break;
    }
  }

  // Calculate the total entropy each gray-level and find the threshold that
  // maximizes it

  double min_ent = itk::NumericTraits<double>::max();

  for (int it = first_bin; it <= last_bin; ++it)
  {
    // Entropy of the background pixels
    double ent_back = 0.0;
    double term = 0.5 / P1[it];
    for (int ih = 1; ih <= it; ++ih)
    { // 0+1?
      ent_back -= norm_histo[ih] * std::log(1.0 - term * P1[ih - 1]);
    }
    ent_back *= term;

    // Entropy of the object pixels
    double ent_obj = 0.0;
    term = 0.5 / P2[it];
    for (int ih = it + 1; static_cast<unsigned int>(ih) < size; ++ih)
    {
      ent_obj -= norm_histo[ih] * std::log(1.0 - term * P2[ih]);
    }
    ent_obj *= term;

    // Total entropy
    const double tot_ent = itk::Math::abs(ent_back - ent_obj);

    if (tot_ent < min_ent)
    {
      min_ent = tot_ent;
      threshold = it;
    }
  }

  this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(threshold, 0)));
}

} // end namespace itk

#endif
