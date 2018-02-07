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
#include "itkProgressReporter.h"
#include "itkNumericTraits.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk {

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject, typename TFunction>
EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject, TFunction >
::EigenToMeasureImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject, typename TFunction>
EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject, TFunction >
::~EigenToMeasureImageFilter()
{}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject, typename TFunction>
void
EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject, TFunction >
::BeforeThreadedGenerateData() ITK_OVERRIDE
{
  /* Set functor parameters after a call to Update() to make sure the input parameters resolve */
  this->GetFunctor().SetParameters( this->GetParametersInput()->Get() );
}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject, typename TFunction>
void
EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject, TFunction >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  InputImageConstPointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput(0);
  SpatialObjectConstPointer maskPointer = this->GetMaskingSpatialObject();
  typename InputImageType::PointType point;

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
      outputIt.Set( m_Functor( inputIt.Get() ) );
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

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject, typename TFunction>
void
EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject, TFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Parameters: " << this->GetParametersInput() << std::endl;
}

} /* end namespace itk */

#endif /* itkEigenToMeasureImageFilter_hxx */
