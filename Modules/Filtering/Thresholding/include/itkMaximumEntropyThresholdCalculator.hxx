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

#ifndef itkMaximumEntropyThresholdCalculator_hxx
#define itkMaximumEntropyThresholdCalculator_hxx

#include "itkMaximumEntropyThresholdCalculator.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template<typename THistogram, typename TOutput>
void
MaximumEntropyThresholdCalculator<THistogram, TOutput>
::GenerateData(void)
{
  const HistogramType * histogram = this->GetInput();

  if ( histogram->GetTotalFrequency() == 0 )
    {
    itkExceptionMacro(<< "Histogram is empty");
    }
  ProgressReporter progress(this, 0, histogram->GetSize(0) );
  if( histogram->GetSize(0) == 1 )
    {
    this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement(0,0) ) );
    }

  unsigned int size = histogram->GetSize(0);

  int threshold = -1;
  int ih, it;
  int first_bin;
  int last_bin;
  double tot_ent;  // total entropy
  double max_ent;  // max entropy
  double ent_back; // entropy of the background pixels at a given threshold
  double ent_obj;  // entropy of the object pixels at a given threshold
  std::vector<double> norm_histo(size); // normalized histogram
  std::vector<double> P1(size); // cumulative normalized histogram
  std::vector<double> P2(size);

  const double tolerance = itk::NumericTraits<double>::epsilon();

  int total = histogram->GetTotalFrequency();

  for( ih = 0; (unsigned)ih < size; ih++ )
    {
    norm_histo[ih] = (double)histogram->GetFrequency(ih, 0) / total;
    }

  P1[0] = norm_histo[0];
  P2[0] = 1.0 - P1[0];
  for( ih = 1; (unsigned)ih < size; ih++ )
    {
    P1[ih] = P1[ih-1] + norm_histo[ih];
    P2[ih] = 1.0 - P1[ih];
    }

  // Determine the first non-zero bin
  first_bin = 0;
  for( ih = 0; (unsigned)ih < size; ih++ )
    {
    if ( !(std::abs(P1[ih]) < tolerance) )
      {
      first_bin = ih;
      break;
      }
    }

  // Determine the last non-zero bin
  last_bin = size - 1;
  for( ih = size - 1; ih >= first_bin; ih-- )
    {
    if ( !(std::abs(P2[ih]) < tolerance) )
      {
      last_bin = ih;
      break;
      }
    }

  // Calculate the total entropy each gray-level and find the threshold that
  // maximizes it
  max_ent = itk::NumericTraits<double>::min();

  for ( it = first_bin; it <= last_bin; it++ )
    {
    // Entropy of the background pixels
    ent_back = 0.0;
    for ( ih = 0; ih <= it; ih++ )
      {
      if ( histogram->GetFrequency(ih, 0) != 0 )
        {
        ent_back -= ( norm_histo[ih] / P1[it] ) * std::log ( norm_histo[ih] / P1[it] );
        }
      }

    // Entropy of the object pixels
    ent_obj = 0.0;
    for ( ih = it + 1; (unsigned)ih < size; ih++ )
      {
      if (histogram->GetFrequency(ih, 0) != 0)
        {
        ent_obj -= ( norm_histo[ih] / P2[it] ) * std::log ( norm_histo[ih] / P2[it] );
        }
      }

    // Total entropy
    tot_ent = ent_back + ent_obj;

    // IJ.log(""+max_ent+"  "+tot_ent);

    const double tol = 0.00001;

    if ( max_ent < (tot_ent - tol) )
      {
      max_ent = tot_ent;
      threshold = it;
      }
    }
  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( threshold, 0 ) ) );

}

} // end namespace itk

#endif
