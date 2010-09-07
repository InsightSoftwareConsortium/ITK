/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunctionImageEvaluatorFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialFunctionImageEvaluatorFilter_txx
#define __itkSpatialFunctionImageEvaluatorFilter_txx

#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"

namespace itk
{
template< class TSpatialFunction, class TInputImage, class TOutputImage >
SpatialFunctionImageEvaluatorFilter< TSpatialFunction, TInputImage, TOutputImage >
::SpatialFunctionImageEvaluatorFilter()
{
  itkDebugMacro(<< "SpatialFunctionImageEvaluatorFilter::SpatialFunctionImageEvaluatorFilter() called");

  // Set the internal function to null
  this->m_PixelFunction = 0;
}

template< class TSpatialFunction, class TInputImage, class TOutputImage >
void
SpatialFunctionImageEvaluatorFilter< TSpatialFunction, TInputImage, TOutputImage >
::GenerateData()
{
  itkDebugMacro(<< "SpatialFunctionImageEvaluatorFilter::GenerateData() called");

  // Allocate the output image
  typename TOutputImage::Pointer outputPtr = this->GetOutput();
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator< TOutputImage > OutputIterator;

  OutputIterator outIt = OutputIterator( outputPtr,
                                         outputPtr->GetRequestedRegion() );

  // The value produced by the spatial function
  // The type is the range of the spatial function
  typename TSpatialFunction::OutputType value;

  // The position at which the function is evaluated
  // The type is the domain of the spatial function
  typename TSpatialFunction::InputType evalPoint;

  // Walk the output image, evaluating the spatial function at each pixel
  for (; !outIt.IsAtEnd(); ++outIt )
    {
    typename TOutputImage::IndexType index = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint(index, evalPoint);
    value = m_PixelFunction->Evaluate(evalPoint);

    // Set the pixel value to the function value
    outIt.Set( (PixelType)value );
    }

  itkDebugMacro(<< "SpatialFunctionImageEvaluatorFilter::GenerateData() finished");
}
} // end namespace

#endif
