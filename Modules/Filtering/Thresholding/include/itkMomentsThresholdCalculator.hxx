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

#ifndef itkMomentsThresholdCalculator_hxx
#define itkMomentsThresholdCalculator_hxx

#include "itkMomentsThresholdCalculator.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template<typename THistogram, typename TOutput>
void
MomentsThresholdCalculator<THistogram, TOutput>
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

  double total = histogram->GetTotalFrequency();
  double m0 = 1.0, m1= 0.0, m2 = 0.0, m3 = 0.0, sum = 0.0, p0 = 0.0;
  double cd, c0, c1, z0, z1; // auxiliary variables
  int threshold = -1;

  std::vector<double> histo(size);
  for( unsigned i = 0; i < size; i++ )
    {
    histo[i] = (double)(histogram->GetFrequency(i, 0) / total); // normalised histogram
    }

  // Calculate the first, second, and third order moments
  for ( unsigned i = 0; i < size; i++ )
    {
    double m = histogram->GetMeasurement(i, 0);
    m1 += m * histo[i];
    m2 += m * m * histo[i];
    m3 += m * m * m * histo[i];
    progress.CompletedPixel();
    }
  //
  // First 4 moments of the gray-level image should match the first 4 moments
  // of the target binary image. This leads to 4 equalities whose solutions
  // are given in the Appendix of Ref. 1
  //
  cd = m0 * m2 - m1 * m1;
  c0 = ( -m2 * m2 + m1 * m3 ) / cd;
  c1 = ( m0 * -m3 + m2 * m1 ) / cd;
  z0 = 0.5 * ( -c1 - std::sqrt ( c1 * c1 - 4.0 * c0 ) );
  z1 = 0.5 * ( -c1 + std::sqrt ( c1 * c1 - 4.0 * c0 ) );
  p0 = ( z1 - m1 ) / ( z1 - z0 );  // Fraction of the object pixels in the target binary image

  // The threshold is the gray-level closest to the p0-tile of the normalized
  // histogram
  sum = 0;
  for( unsigned i = 0; i < size; i++ )
    {
    sum += histo[i];
    if (sum>p0)
      {
      threshold = i;
      break;
      }
    }

  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( threshold, 0 ) ) );
}

} // end namespace itk

#endif
