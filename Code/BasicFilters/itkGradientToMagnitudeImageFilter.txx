/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientToMagnitudeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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

template< class TInputImage, class TOutputImage, class TComputation >
GradientToMagnitudeImageFilter< TInputImage, TOutputImage, TComputation >
::GradientToMagnitudeImageFilter()
{
  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GradientToMagnitudeImageFilter() called");

}


template< class TInputImage, class TOutputImage, class TComputation >
void
GradientToMagnitudeImageFilter< TInputImage, TOutputImage, TComputation >
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
    TComputation acc = 0;

    // determine the index of the output pixel
    index = outIt.GetIndex();

    for(int i = 0; i < NDimensions; i++)
      {
      acc += static_cast<TComputation>(inputPtr->GetPixel(index)[i])
        * static_cast<TComputation>(inputPtr->GetPixel(index)[i]);
      }

    acc = sqrt(acc);

    outputPtr->GetPixel(index) = static_cast<OutputImagePixelType>(acc);
    }

  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GenerateData() finished");
}

template< class TInputImage, class TOutputImage, class TComputation >
void
GradientToMagnitudeImageFilter< TInputImage, TOutputImage, TComputation >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace

#endif
