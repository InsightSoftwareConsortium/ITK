/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientToMagnitudeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkGradientToMagnitudeImageFilter_txx
#define __itkGradientToMagnitudeImageFilter_txx

#include <iostream>
#include <math.h>
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkGradientToMagnitudeImageFilter.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
GradientToMagnitudeImageFilter< TInputImage, TOutputImage >
::GradientToMagnitudeImageFilter()
{
  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GradientToMagnitudeImageFilter() called");

}


template< class TInputImage, class TOutputImage >
void
GradientToMagnitudeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GenerateData() called");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType index;

  // walk the output image, and sample the input image
  for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
    {
    double acc = 0;

    // determine the index of the output pixel
    index = outIt.GetIndex();

    for(int i = 0; i < NDimensions; i++)
      {
      acc += inputPtr->GetPixel(index)[i]
        * inputPtr->GetPixel(index)[i];
      }

    acc = sqrt(acc);

    outputPtr->GetPixel(index) = acc;
    }

  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GenerateData() finished");
}

} // end namespace

#endif
