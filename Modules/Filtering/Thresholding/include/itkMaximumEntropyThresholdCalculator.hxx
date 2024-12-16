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

#ifndef itkMaximumEntropyThresholdCalculator_hxx
#define itkMaximumEntropyThresholdCalculator_hxx

#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template <typename THistogram, typename TOutput>
void
MaximumEntropyThresholdCalculator<THistogram, TOutput>::GenerateData()
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

  constexpr double tolerance = itk::NumericTraits<double>::epsilon();
  const int        total = histogram->GetTotalFrequency();

  std::vector<double> norm_histo(size); // normalized histogram
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
  double max_ent = itk::NumericTraits<double>::min(); // max entropy

  for (int it = first_bin; it <= last_bin; ++it)
  {
    // Entropy of the background pixels
    double ent_back = 0.0; // entropy of the background pixels at a given threshold
    for (int ih = 0; ih <= it; ++ih)
    {
      if (histogram->GetFrequency(ih, 0) != 0)
      {
        ent_back -= (norm_histo[ih] / P1[it]) * std::log(norm_histo[ih] / P1[it]);
      }
    }

    // entropy of the object pixels at a given threshold
    double ent_obj = 0.0;
    for (int ih = it + 1; static_cast<unsigned int>(ih) < size; ++ih)
    {
      if (histogram->GetFrequency(ih, 0) != 0)
      {
        ent_obj -= (norm_histo[ih] / P2[it]) * std::log(norm_histo[ih] / P2[it]);
      }
    }

    const double tot_ent = ent_back + ent_obj; // total entropy
    // IJ.log(""+max_ent+"  "+tot_ent);

    constexpr double tol = 0.00001;

    if (max_ent < (tot_ent - tol))
    {
      max_ent = tot_ent;
      threshold = it;
    }
  }
  this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(threshold, 0)));
}

} // end namespace itk

#endif
