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

#ifndef __itkHuangThresholdCalculator_hxx
#define __itkHuangThresholdCalculator_hxx

#include "itkHuangThresholdCalculator.h"
#include "itkMath.h"
#include "itkProgressReporter.h"
#include "vnl/vnl_math.h"

namespace itk
{

/*
 * Compute the Huang's threshold
 */
template<class THistogram, class TOutput>
void
HuangThresholdCalculator<THistogram, TOutput>
::GenerateData(void)
{
  const HistogramType * histogram = this->GetInput();
  // histogram->Print(std::cout);
  if ( histogram->GetTotalFrequency() == 0 )
    {
    itkExceptionMacro(<< "Histogram is empty");
    }
  ProgressReporter progress(this, 0, histogram->GetSize(0) );
  if( histogram->GetSize(0) == 1 )
    {
    this->GetOutput()->Set( histogram->GetMeasurement(0,0) );
    }

  int size = histogram->GetSize(0);
  // find first and last non-empty bin - could replace with stl
  int first = 0;
  while( first < size && histogram->GetFrequency(first, 0) == 0 )
    {
    first++;
    }
  if (first == size)
    {
    itkWarningMacro(<< "No data in histogram");
    return;
    }
  int last = size - 1;
  while( last > first && histogram->GetFrequency(last, 0) == 0)
    {
    last--;
    }

  // calculate the cumulative density and the weighted cumulative density
  std::vector<double> S(last+1, 0.0);
  std::vector<double> W(last+1, 0.0);

  S[0] = histogram->GetFrequency(0, 0);

  for( int i = std::max((int)1, first); i <= last; i++ )
    {
    S[i] = S[i - 1] + histogram->GetFrequency(i, 0);
    W[i] = W[i - 1] + histogram->GetMeasurement(i, 0) * histogram->GetFrequency(i, 0);
    }

  // precalculate the summands of the entropy given the absolute difference x - mu (integral)
  double C = last - first;
  std::vector<double> Smu(last + 1 - first, 0);

  for( unsigned int i = 1; i < Smu.size(); i++)
    {
    double mu = 1 / (1 + i / C);
    Smu[i] = -mu * vcl_log(mu) - (1 - mu) * vcl_log(1 - mu);
    }

  // calculate the threshold
  int bestThreshold = 0;
  double bestEntropy = itk::NumericTraits<double>::max();
  for( int threshold = first; threshold <= last; threshold++ )
    {
    double entropy = 0;
    int mu = Math::Round<int>(W[threshold] / S[threshold]);
    typename HistogramType::MeasurementVectorType v(1);
    v[0]=mu;
    itk::IndexValueType muIdx = histogram->GetIndex(v)[0];
    for( int i = first; i <= threshold; i++ )
      {
      entropy += Smu[vcl_abs(i - muIdx)] * histogram->GetFrequency(i, 0);
      }
    mu = Math::Round<int>((W[last] - W[threshold]) / (S[last] - S[threshold]));
    v[0]=mu;
    muIdx = histogram->GetIndex(v)[0];
    for( int i = threshold + 1; i <= last; i++ )
      {
      entropy += Smu[vcl_abs(i - muIdx)] * histogram->GetFrequency(i, 0);
      }
    if (bestEntropy > entropy)
      {
      bestEntropy = entropy;
      bestThreshold = threshold;
      }
    }

  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( bestThreshold, 0 ) ) );
}

} // end namespace itk

#endif
