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
#ifndef itkFFTNormalizedCorrelationImageFilter_hxx
#define itkFFTNormalizedCorrelationImageFilter_hxx

#include "itkFFTNormalizedCorrelationImageFilter.h"

namespace itk
{
template<typename TInputImage, typename TOutputImage>
void FFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Generate the output for this filter by calling the more general
  // itkMaskedFFTNormalizedCorrelationImageFilter.  If the masks for
  // this filter are not set or are set to images of ones, the results
  // will be the standard FFT NCC.

  // call the superclass' implementation of this method
  Superclass::GenerateData();
}

template< typename TInputImage, typename TOutputImage >
void
FFTNormalizedCorrelationImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
