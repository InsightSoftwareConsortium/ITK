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
#ifndef __itkIntermodesThresholdCalculator_hxx
#define __itkIntermodesThresholdCalculator_hxx

#include "itkIntermodesThresholdCalculator.h"
#include "itkProgressReporter.h"
#include "vnl/vnl_math.h"

namespace itk
{

template<class THistogram, class TOutput>
bool
IntermodesThresholdCalculator<THistogram, TOutput>
::BimodalTest(const std::vector<double> & h)
{
  int modes = 0;

  const unsigned len = h.size();
  for (unsigned k = 1; k < len - 1; k++)
    {
    if (h[k-1] < h[k] && h[k+1] < h[k])
      {
      modes++;
      if (modes>2)
        return false;
      }

    }

  return (modes == 2);
}

/*
 * Compute the Intermodes's threshold
 */
template<class THistogram, class TOutput>
void
IntermodesThresholdCalculator<THistogram, TOutput>
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
    this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement(0,0) ) );
    }

  unsigned int size = histogram->GetSize(0);

  // smooth the histogram
  std::vector<double> smoothedHist;
  smoothedHist.resize(size);
  for( unsigned int i = 0; i<size; i++)
    {
    smoothedHist[i] = histogram->GetFrequency(i, 0);
    progress.CompletedPixel();
    }

  unsigned smIter = 0;

  while (!BimodalTest(smoothedHist))
    {
    // smooth with a 3 point running mean
    double previous = 0, current = 0, next = smoothedHist[0];
    for (unsigned i = 0; i < smoothedHist.size() - 1; i++)
      {
      previous = current;
      current = next;
      next = smoothedHist[i + 1];
      smoothedHist[i] = (previous + current + next) / 3;
      }
    smoothedHist[smoothedHist.size() - 1] = (current + next) / 3;
    smIter++;
    if (smIter > m_MaximumSmoothingIterations )
      {
      itkExceptionMacro( << "Exceeded maximum iterations for histogram smoothing." );
      return;
      }
    }
  if (m_UseInterMode)
    {
    // The threshold is the mean between the two peaks.
    unsigned tt=0;
    for (unsigned i=1; i<smoothedHist.size() - 1; i++)
      {
      if (smoothedHist[i-1] < smoothedHist[i] && smoothedHist[i+1] < smoothedHist[i])
        {
        tt += i;
        }
      }
    this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( tt/2, 0 ) ) );
    }
  else
    {
    unsigned firstpeak=0;
    for (unsigned i=1; i<smoothedHist.size() - 1; i++)
      {
      if (smoothedHist[i-1] < smoothedHist[i] && smoothedHist[i+1] < smoothedHist[i])
        {
        firstpeak = i;
        break;
        }
      }
    double minVal = smoothedHist[firstpeak];
    unsigned minPos = firstpeak;

    for (unsigned i=firstpeak + 1; i<smoothedHist.size() - 1; i++)
      {
      if (smoothedHist[i] < minVal)
        {
        minVal = smoothedHist[i];
        minPos = i;
        }
      if (smoothedHist[i-1] < smoothedHist[i] && smoothedHist[i+1] < smoothedHist[i])
        {
        break;
        }
      }
    this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( minPos, 0 ) ) );
    }
}

template<class THistogram, class TOutput>
void
IntermodesThresholdCalculator<THistogram, TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "MaximumSmoothingIterations: " << m_MaximumSmoothingIterations << std::endl;
  os << indent << "UseInterMode: " << m_UseInterMode << std::endl;
}

} // end namespace itk

#endif
