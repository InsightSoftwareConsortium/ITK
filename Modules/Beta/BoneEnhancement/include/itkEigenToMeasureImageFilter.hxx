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

#ifndef itkEigenToMeasureImageFilter_hxx
#define itkEigenToMeasureImageFilter_hxx

#include "itkEigenToMeasureImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk {

template< typename TInputImage, typename TOutputImage >
void
EigenToMeasureImageFilter< TInputImage, TOutputImage >
::DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread)
{
  /* Get Inputs */
  InputImageConstPointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);
  MaskSpatialObjectTypeConstPointer maskPointer = this->GetMask();
  typename InputImageType::PointType point;

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators
  ImageRegionConstIteratorWithIndex< TInputImage >  inputIt(inputPtr, inputRegionForThread);
  ImageRegionIterator< TOutputImage >               outputIt(outputPtr, outputRegionForThread);

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
  {
    inputPtr->TransformIndexToPhysicalPoint(inputIt.GetIndex(), point);
    if ( (!maskPointer) ||  (maskPointer->IsInside(point)) )
    {
      outputIt.Set( ProcessPixel( inputIt.Get() ) );
    }
    else
    {
      outputIt.Set( NumericTraits< OutputImagePixelType >::Zero );
    }
    ++inputIt;
    ++outputIt;
  }
}

} /* end namespace */

#endif /* itkEigenToMeasureImageFilter_hxx */
