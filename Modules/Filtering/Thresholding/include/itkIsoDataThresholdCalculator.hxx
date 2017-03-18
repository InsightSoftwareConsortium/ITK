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

#ifndef itkIsoDataThresholdCalculator_hxx
#define itkIsoDataThresholdCalculator_hxx

#include "itkIsoDataThresholdCalculator.h"
#include "itkMath.h"
#include "itkProgressReporter.h"

namespace itk
{

template<typename THistogram, typename TOutput>
void
IsoDataThresholdCalculator<THistogram, TOutput>
::GenerateData(void)
{
  const HistogramType * histogram = this->GetInput();

  if ( histogram->GetTotalFrequency() == 0 )
    {
    itkExceptionMacro(<< "Histogram is empty");
    }
  SizeValueType size = histogram->GetSize(0);
  ProgressReporter progress(this, 0, size );
  if( size == 1 )
    {
    this->GetOutput()->Set( static_cast< OutputType >( histogram->GetMeasurement(0,0) ) );
    return;
    }

  InstanceIdentifier currentPos = 0;
  while (true)
    {
    // Skip the empty bins to speed up things
    for( InstanceIdentifier i = currentPos; i < size; i++)
      {
      if( histogram->GetFrequency(i, 0) > 0 )
        {
        currentPos = i;
        break;
        }
      progress.CompletedPixel();
      }
    if( currentPos >= size )
      {
      // Can't compute the isodata value - use the mean instead
      this->GetOutput()->Set( static_cast<OutputType>( histogram->Mean(0) ) );
      return;
      }
    // Compute the mean of the lower values
    double l = 0;
    double totl = 0;

    for( InstanceIdentifier i = 0; i <= currentPos; i++ )
      {
      totl += static_cast< double >( histogram->GetFrequency(i, 0) );
      l += static_cast< double >( histogram->GetMeasurement(i, 0) ) * static_cast< double >( histogram->GetFrequency(i, 0) );
      }
    // Compute the mean of the higher values
    double h = 0;
    double toth = 0;

    for( InstanceIdentifier i = currentPos + 1; i < size; i++ )
      {
      toth += static_cast< double >( histogram->GetFrequency(i, 0) );
      h += static_cast< double >( histogram->GetMeasurement(i, 0) ) * static_cast< double >( histogram->GetFrequency(i, 0) );
      }
    // Avoid a potential division by 0
    if( ( totl > itk::Math::eps ) && ( toth > itk::Math::eps ) )
      {
      l /= totl;
      h /= toth;

      if( histogram->GetMeasurement( currentPos, 0 ) >= (l + h) * 0.5 )
        {
        this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( currentPos, 0 ) ) );
        return;
        }
      }

    ++currentPos;
    progress.CompletedPixel();
    }
}

} // end namespace itk

#endif
