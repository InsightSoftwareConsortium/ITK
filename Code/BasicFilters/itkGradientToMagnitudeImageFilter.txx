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

#include <math.h>
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkImageRegionConstIterator.h"
#include "itkGradientToMagnitudeImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
GradientToMagnitudeImageFilter< TInputImage, TOutputImage >
::GradientToMagnitudeImageFilter()
{
  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GradientToMagnitudeImageFilter() called");

}


template< typename TInputImage, typename TOutputImage >
void
GradientToMagnitudeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GenerateData() called");

  // Get the input and output pointers
  InputImageConstPointer  inputPtr  = this->GetInput(0);
  OutputImagePointer      outputPtr = this->GetOutput(0);

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create an iterator that will walk the output region
  typedef ImageRegionConstIterator<TInputImage> InputIterator;
  typedef ImageRegionIterator<TOutputImage>     OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  InputIterator   inIt = InputIterator(inputPtr,
                                        inputPtr->GetRequestedRegion());

  inIt.GoToBegin();

  // walk the output image, and sample the input image
  for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt, ++inIt )
    {
    RealType acc = NumericTraits< RealType >::Zero;

    for(unsigned int i = 0; i < NDimensions; i++)
      {
      const RealType component = static_cast<RealType>( inIt.Get()[i]);
      acc += component * component;
      }

    acc = sqrt( acc );

    outIt.Set( static_cast<OutputImagePixelType>(acc) );
    }

  itkDebugMacro(<< "GradientToMagnitudeImageFilter::GenerateData() finished");
}

template< typename TInputImage, typename TOutputImage >
void
GradientToMagnitudeImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace

#endif
