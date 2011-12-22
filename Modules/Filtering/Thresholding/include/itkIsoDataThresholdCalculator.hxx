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

#ifndef __itkIsoDataThresholdCalculator_hxx
#define __itkIsoDataThresholdCalculator_hxx

#include "itkIsoDataThresholdCalculator.h"
#include "vnl/vnl_math.h"
#include "itkProgressReporter.h"

namespace itk
{

/*
 * Compute the IsoData's threshold
 */
template<class THistogram, class TOutput>
void
IsoDataThresholdCalculator<THistogram, TOutput>
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

  unsigned int currentPos = 0;
  while (true)
    {
    // std::cout << "currentPos: " << currentPos << std::endl;
    // skip the empty bins to speed up things
    for( unsigned int i = currentPos; i < histogram->GetSize(0); i++)
      {
      if( histogram->GetFrequency(i, 0) > 0 )
        {
        currentPos = i;
        break;
        }
      progress.CompletedPixel();
      }
    if( currentPos >= histogram->GetSize(0) )
      {
      // can't compute the isodata value - use the mean instead
      this->GetOutput()->Set( static_cast<OutputType>( histogram->Mean(0) ) );
      return;
      }
    // compute the mean of the lower values
    double l = 0;
    double totl = 0;
    for( unsigned i = 0; i <= currentPos; i++)
      {
      totl += histogram->GetFrequency(i, 0);
      l += histogram->GetMeasurement(i, 0) * histogram->GetFrequency(i, 0);
      }
    // compute the mean of the higher values
    double h = 0;
    double toth = 0;
    for( unsigned int i = currentPos + 1; i < histogram->GetSize(0); i++)
      {
      toth += histogram->GetFrequency(i, 0);
      h += histogram->GetMeasurement(i, 0) * histogram->GetFrequency(i, 0);
      }
    // a test to avoid a potential division by 0
    // std::cout << "totl: " << totl << std::endl;
    // std::cout << "toth: " << toth << std::endl;
    if (totl > 0 && toth > 0)
      {
      l /= totl;
      h /= toth;
      // std::cout << "histogram->GetMeasurement( currentPos, 0 ): " << histogram->GetMeasurement( currentPos, 0 ) << std::endl;
      // std::cout << "l: " << l << std::endl;
      // std::cout << "h: " << h << std::endl;
      // std::cout << "(l + h) / 2: " << (l + h) / 2 << std::endl;
      if( histogram->GetMeasurement( currentPos, 0 ) >= (l + h) / 2 )
        {
        this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( currentPos, 0 ) ) );
        return;
        }
      }
    currentPos++;
    progress.CompletedPixel();
    }
}

} // end namespace itk

#endif
