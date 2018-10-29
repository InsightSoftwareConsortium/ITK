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

#ifndef itkKrcahEigenToMeasureImageFilter_hxx
#define itkKrcahEigenToMeasureImageFilter_hxx

#include "itkKrcahEigenToMeasureImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk {
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::KrcahEigenToMeasureImageFilter() :
  Superclass(),
  m_EnhanceType(-1.0f)
{}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
void
KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId)
{
  /* Get Inputs */
  ParameterArrayType parameters = this->GetParametersInput()->Get();
  InputImageConstPointer inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);
  SpatialObjectConstPointer maskPointer = this->GetMaskingSpatialObject();
  typename InputImageType::PointType point;

  /* Set parameters */
  if (parameters.GetSize() != 3)
  {
    itkExceptionMacro(<< "Parameters must have size 3. Given array of size " << parameters.GetSize());
  }

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators
  ImageRegionConstIteratorWithIndex< TInputImage >  inputIt(inputPtr, inputRegionForThread);
  ImageRegionIterator< TOutputImage >               outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
  {
    inputPtr->TransformIndexToPhysicalPoint(inputIt.GetIndex(), point);
    if ( (!maskPointer) ||  (maskPointer->IsInside(point)) )
    {
      outputIt.Set( m_Functor( inputIt.Get()) );
    }
    else
    {
      outputIt.Set( NumericTraits< OutputImagePixelType >::Zero );
    }
    ++inputIt;
    ++outputIt;
    progress.CompletedPixel();  // potential exception thrown here
  }
}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
void
KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Direction: " << GetEnhanceType() << std::endl;
}

} /* end namespace */

#endif /* itkKrcahEigenToMeasureImageFilter_hxx */
