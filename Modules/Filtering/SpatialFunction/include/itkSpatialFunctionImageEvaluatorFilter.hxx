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
#ifndef itkSpatialFunctionImageEvaluatorFilter_hxx
#define itkSpatialFunctionImageEvaluatorFilter_hxx

#include "itkImageRegion.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"

namespace itk
{
template< typename TSpatialFunction, typename TInputImage, typename TOutputImage >
SpatialFunctionImageEvaluatorFilter< TSpatialFunction, TInputImage, TOutputImage >
::SpatialFunctionImageEvaluatorFilter()
{
  itkDebugMacro(<< "SpatialFunctionImageEvaluatorFilter::SpatialFunctionImageEvaluatorFilter() called");

  // Set the internal function to null
  this->m_PixelFunction = ITK_NULLPTR;
}

template< typename TSpatialFunction, typename TInputImage, typename TOutputImage >
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
