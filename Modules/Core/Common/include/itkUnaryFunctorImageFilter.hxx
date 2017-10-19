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
#ifndef itkUnaryFunctorImageFilter_hxx
#define itkUnaryFunctorImageFilter_hxx

#include "itkUnaryFunctorImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage, typename TFunction  >
UnaryFunctorImageFilter< TInputImage, TOutputImage, TFunction >
::UnaryFunctorImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->InPlaceOff();
}

/**
 * UnaryFunctorImageFilter can produce an image which is a different resolution
 * than its input image.  As such, UnaryFunctorImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
template< typename TInputImage, typename TOutputImage, typename TFunction >
void
UnaryFunctorImageFilter< TInputImage, TOutputImage, TFunction >
::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
  OutputImageType *outputPtr = this->GetOutput();
  const InputImageType *inputPtr  = this->GetInput();

  if ( !outputPtr || !inputPtr )
    {
    return;
    }

  // Set the output image largest possible region.  Use a RegionCopier
  // so that the input and output images can be different dimensions.
  OutputImageRegionType outputLargestPossibleRegion;
  this->CallCopyInputRegionToOutputRegion( outputLargestPossibleRegion,
                                           inputPtr->GetLargestPossibleRegion() );
  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);

  ImageToImageFilterDetail::ImageInformationCopier<Superclass::OutputImageDimension,
                                                   Superclass::InputImageDimension>
    informationCopier;
  informationCopier(outputPtr, inputPtr);
}

/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template< typename TInputImage, typename TOutputImage, typename TFunction  >
void
UnaryFunctorImageFilter< TInputImage, TOutputImage, TFunction >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const typename OutputImageRegionType::SizeType &regionSize = outputRegionForThread.GetSize();

  if( regionSize[0] == 0 )
    {
    return;
    }
  const TInputImage *inputPtr = this->GetInput();
  TOutputImage *outputPtr = this->GetOutput(0);

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  const SizeValueType numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / regionSize[0];
  ProgressReporter progress( this, threadId, numberOfLinesToProcess );

  // Define the iterators
  ImageScanlineConstIterator< TInputImage > inputIt(inputPtr, inputRegionForThread);
  ImageScanlineIterator< TOutputImage > outputIt(outputPtr, outputRegionForThread);

  inputIt.GoToBegin();
  outputIt.GoToBegin();
  while ( !inputIt.IsAtEnd() )
    {
    while ( !inputIt.IsAtEndOfLine() )
      {
      outputIt.Set( m_Functor( inputIt.Get() ) );
      ++inputIt;
      ++outputIt;
      }
    inputIt.NextLine();
    outputIt.NextLine();
    progress.CompletedPixel();  // potential exception thrown here
    }
}
} // end namespace itk

#endif
