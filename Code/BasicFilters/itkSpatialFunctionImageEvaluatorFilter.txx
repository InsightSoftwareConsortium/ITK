/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunctionImageEvaluatorFilter.txx
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
#ifndef __itkSpatialFunctionImageEvaluatorFilter_txx
#define __itkSpatialFunctionImageEvaluatorFilter_txx

#include <iostream>
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"

namespace itk
{

template< class TSpatialFunction, class TInputImage, class TOutputImage >
SpatialFunctionImageEvaluatorFilter< TSpatialFunction, TInputImage, TOutputImage >
::SpatialFunctionImageEvaluatorFilter()
{
  std::cout << "SpatialFunctionImageEvaluatorFilter::SpatialFunctionImageEvaluatorFilter() called\n";
  
  // Set the internal function to null
  this->m_pFunction = 0;
}


template< class TSpatialFunction, class TInputImage, class TOutputImage >
void
SpatialFunctionImageEvaluatorFilter< TSpatialFunction, TInputImage, TOutputImage >
::GenerateData()
{
  itkDebugMacro(<< "SpatialFunctionImageEvaluatorFilter::GenerateData() called");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

  // Make sure we're getting everything
  inputPtr->SetRequestedRegionToLargestPossibleRegion();

  // How big is the input image?
  typename TInputImage::SizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

  // Create a region object native to the output image type
  OutputImageRegionType outputRegion;

  // Resize the output region
  outputRegion.SetSize( inputSize );

  // Set the largest legal region size (i.e. the size of the whole image)
  // to what we just defined
  outputPtr->SetLargestPossibleRegion( outputRegion );
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->SetRequestedRegion( outputRegion );
  outputPtr->Allocate();

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType outputIndex;

  const double* origin;
  const double* spacing;

  // Get the origin and spacing from the input image
  origin = inputPtr->GetOrigin();
  spacing = inputPtr->GetSpacing();

  // Set the origin and spacing of the output image
  outputPtr->SetOrigin(origin);
  outputPtr->SetSpacing(spacing);

  TPositionType outputPosition;

  double value;

  // walk the output image, and sample the input image
  for ( ; !outIt.IsAtEnd(); ++outIt)
    {
    // determine the index of the output pixel
    outputPosition = outIt.GetIndex();

    for (int ii = 0; ii < NDimensions; ++ii)
      outputPosition[ii] = outputIndex[ii]*spacing[ii]+origin[ii];

    value = m_pFunction->Evaluate(&outputVector);

    // Set the pixel value to the function value
    outIt.Set( (PixelType) value);
    
    } // end switch

  std::cout << "SpatialFunctionImageEvaluatorFilter::GenerateData() finished\n";
}

} // end namespace

#endif
