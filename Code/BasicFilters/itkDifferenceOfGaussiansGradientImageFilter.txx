/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDifferenceOfGaussiansGradientImageFilter.txx
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
#ifndef __itkDifferenceOfGaussiansGradientImageFilter_txx
#define __itkDifferenceOfGaussiansGradientImageFilter_txx

#include <iostream>
#include <math.h>
#include "itkDifferenceOfGaussiansGradientImageFilter.h"

namespace itk
{
template<typename TInputImage, typename TDataType>
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::DifferenceOfGaussiansGradientImageFilter()
{
  itkDebugMacro(<< "DifferenceOfGaussiansGradientImageFilter::DifferenceOfGaussiansGradientImageFilter() called");

  m_Width = 2;
}

template<typename TInputImage, typename TDataType>
void
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::GenerateData()
{
  itkDebugMacro(<< "DifferenceOfGaussiansGradientImageFilter::GenerateData() called");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

  // Make sure we're getting everything
  inputPtr->SetRequestedRegionToLargestPossibleRegion();

  // How big is the input image?
  typename TInputImage::SizeType size = inputPtr->GetLargestPossibleRegion().GetSize();

  // Create a region object native to the output image type
  OutputImageRegionType outputRegion;

  // Resize the output region
  outputRegion.SetSize( size );

  // Set the largest legal region size (i.e. the size of the whole image)
  // to what we just defined
  outputPtr->SetLargestPossibleRegion( outputRegion );
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->SetRequestedRegion( outputRegion );
  outputPtr->Allocate();

  // Pass through the origin and spacing of the source image
  const double* origin;
  const double* spacing;

  // Get the origin and spacing from the input image
  origin = inputPtr->GetOrigin();
  spacing = inputPtr->GetSpacing();

  // Set the origin and spacing of the output image
  outputPtr->SetOrigin(origin);
  outputPtr->SetSpacing(spacing);

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType outputIndex;
  typename TOutputImage::IndexType upperIndex;
  typename TOutputImage::IndexType lowerIndex;

  // walk the output image, and sample the input image
  for ( ; !outIt.IsAtEnd(); ++outIt)
    {
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();

    // is the current index an acceptable distance from the edges
    // of the image?
    bool isValidGrad = true;

    for (int i = 0; i < NDimensions; ++i)
      {
        const int width = static_cast<int>(m_Width);
        const int sizeDifference = 
            static_cast<int>(size.m_Size[i]) - width;
          
        if( !( (outputIndex[i] < sizeDifference ) &&
               (outputIndex[i] >= width         )    ) )
        {
          isValidGrad = false;
        }
      }

    if (isValidGrad)
      {
      // We're in a safe position, so calculate the gradient for
      // each dimension
      for (int i = 0; i < NDimensions; i++)
        {
        // Build the indices for each pixel
        for (int j = 0; j < NDimensions; j++)
          {
          if(j == i)
            {
            upperIndex[j] = outputIndex[j] + static_cast<TOutputImage::IndexValueType>(m_Width);
            lowerIndex[j] = outputIndex[j] - static_cast<TOutputImage::IndexValueType>(m_Width);
            }
          else
            {
            upperIndex[j] = outputIndex[j];
            lowerIndex[j] = outputIndex[j];
            }
          }
        // Remember, output type is a covariant vector of TDataType
        outputPtr->GetPixel(outputIndex)[i] =
        inputPtr->GetPixel(upperIndex) - inputPtr->GetPixel(lowerIndex);
        }
      }
    else // We're not in a safe position, gradient is zero
      {
      for (int i = 0; i < NDimensions; ++i)
        outputPtr->GetPixel(outputIndex)[i] = 0.0;
      }
    }

  itkDebugMacro(<< "DifferenceOfGaussiansGradientImageFilter::GenerateData() finished");
}

template<typename TInputImage, typename TDataType>
void
DifferenceOfGaussiansGradientImageFilter< TInputImage, TDataType >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Width is " << m_Width << std::endl;
}

} // end namespace

#endif
