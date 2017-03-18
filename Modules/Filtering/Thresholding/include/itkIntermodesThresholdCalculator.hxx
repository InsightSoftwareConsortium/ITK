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
#ifndef itkIntermodesThresholdCalculator_hxx
#define itkIntermodesThresholdCalculator_hxx

#include "itkIntermodesThresholdCalculator.h"
#include "itkProgressReporter.h"

namespace itk
{

template<typename THistogram, typename TOutput>
bool
IntermodesThresholdCalculator<THistogram, TOutput>
::BimodalTest(const std::vector<double> & h)
{
  int modes = 0;

  const size_t len = h.size();
  for (size_t k = 1; k < len - 1; k++)
    {
    if ( (h[k-1] < h[k]) && (h[k+1] < h[k]))
      {
      ++modes;
      if(modes > 2)
        {
        return false;
        }
      }
    }

  return (modes == 2);
}

template<typename THistogram, typename TOutput>
void
IntermodesThresholdCalculator<THistogram, TOutput>
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
    this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement(0,0) ) );
    return;
    }

  // Smooth the histogram
  std::vector<double> smoothedHist(size);
  for( InstanceIdentifier i = 0; i<size; i++)
    {
    smoothedHist[i] = static_cast< double >( histogram->GetFrequency(i, 0) );
    progress.CompletedPixel();
    }

  SizeValueType smIter = 0;

  while (!BimodalTest(smoothedHist))
    {
    // Smooth with a 3 point running mean
    double previous = 0.;
    double current = 0.;
    double next = smoothedHist[0];

    for (size_t i = 0; i < smoothedHist.size() - 1; i++)
      {
      previous = current;
      current = next;
      next = smoothedHist[i + 1];
      smoothedHist[i] = (previous + current + next) / 3.;
      }
    smoothedHist[smoothedHist.size() - 1] = (current + next) / 3.;
    ++smIter;

    if (smIter > m_MaximumSmoothingIterations )
      {
      itkGenericExceptionMacro( << "Exceeded maximum iterations for histogram smoothing." );
      return;
      }
    }


  size_t tt = 0;
  if (m_UseInterMode)
    {
    // The threshold is the mean between the two peaks.
    for (size_t i=1; i<smoothedHist.size() - 1; i++)
      {
      if ( ( smoothedHist[i-1] < smoothedHist[i] )&& ( smoothedHist[i+1] < smoothedHist[i] ) )
        {
        tt += i;
        }
      }
    tt /= 2;
    }
  else
    {
    size_t firstpeak = 0;
    for( size_t i = 1; i < smoothedHist.size() - 1; i++ )
      {
      if( (smoothedHist[i-1] < smoothedHist[i] ) && ( smoothedHist[i+1] < smoothedHist[i] ) )
        {
        firstpeak = i;
        break;
        }
      }
    double minVal = smoothedHist[firstpeak];
    tt = firstpeak;

    for( size_t i = firstpeak + 1; i < smoothedHist.size() - 1; i++ )
      {
      if (smoothedHist[i] < minVal)
        {
        minVal = smoothedHist[i];
        tt = i;
        }
      if( (smoothedHist[i-1] < smoothedHist[i] ) && ( smoothedHist[i+1] < smoothedHist[i] ) )
        {
        break;
        }
      }
    }
  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( static_cast<InstanceIdentifier>( tt ), 0 ) ) );
}

template<typename THistogram, typename TOutput>
void
IntermodesThresholdCalculator<THistogram, TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MaximumSmoothingIterations: "
    << static_cast< typename itk::NumericTraits<
    SizeValueType >::PrintType >( m_MaximumSmoothingIterations ) << std::endl;
  os << indent << "UseInterMode: " << m_UseInterMode << std::endl;
}

} // end namespace itk

#endif
