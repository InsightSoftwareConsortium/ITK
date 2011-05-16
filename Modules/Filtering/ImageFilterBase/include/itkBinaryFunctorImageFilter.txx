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
#ifndef __itkBinaryFunctorImageFilter_txx
#define __itkBinaryFunctorImageFilter_txx

#include "itkBinaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::BinaryFunctorImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  this->InPlaceOff();
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput1(const TInputImage1 *image1)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 0, const_cast< TInputImage1 * >( image1 ) );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< class TInputImage1, class TInputImage2,
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::SetInput2(const TInputImage2 *image2)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 1, const_cast< TInputImage2 * >( image2 ) );
}

/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template< class TInputImage1, class TInputImage2, class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage, TFunction >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // We use dynamic_cast since inputs are stored as DataObjects.  The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second input.
  Input1ImagePointer inputPtr1 =
    dynamic_cast< const TInputImage1 * >( ProcessObject::GetInput(0) );
  Input2ImagePointer inputPtr2 =
    dynamic_cast< const TInputImage2 * >( ProcessObject::GetInput(1) );
  OutputImagePointer outputPtr = this->GetOutput(0);

  ImageRegionConstIterator< TInputImage1 > inputIt1(inputPtr1, outputRegionForThread);
  ImageRegionConstIterator< TInputImage2 > inputIt2(inputPtr2, outputRegionForThread);

  ImageRegionIterator< TOutputImage > outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  inputIt1.GoToBegin();
  inputIt2.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt1.IsAtEnd() )
    {
    outputIt.Set( m_Functor( inputIt1.Get(), inputIt2.Get() ) );
    ++inputIt2;
    ++inputIt1;
    ++outputIt;
    progress.CompletedPixel(); // potential exception thrown here
    }
}
} // end namespace itk

#endif
