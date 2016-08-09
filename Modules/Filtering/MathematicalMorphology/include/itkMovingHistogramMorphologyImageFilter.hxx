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
#ifndef itkMovingHistogramMorphologyImageFilter_hxx
#define itkMovingHistogramMorphologyImageFilter_hxx

#include "itkMovingHistogramMorphologyImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram >
MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::MovingHistogramMorphologyImageFilter() :
  m_Boundary( NumericTraits< PixelType >::ZeroValue() )
{
}

template< typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram >
void
MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::ConfigureHistogram(THistogram & histogram)
{
  histogram.SetBoundary(m_Boundary);
}

template< typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram >
void
MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Boundary: " << m_Boundary << std::endl;
}
} // end namespace itk
#endif
