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
#ifndef __itkUnaryFunctorImageFilter_hxx
#define __itkUnaryFunctorImageFilter_hxx

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
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr  = this->GetInput();

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

  // Set the output spacing and origin
  const ImageBase< Superclass::InputImageDimension > *phyData;

  phyData =
    dynamic_cast< const ImageBase< Superclass::InputImageDimension > * >( this->GetInput() );

  if ( phyData )
    {
    // Copy what we can from the image from spacing and origin of the input
    // This logic needs to be augmented with logic that select which
    // dimensions to copy
    unsigned int i, j;
    const typename InputImageType::SpacingType &
    inputSpacing = inputPtr->GetSpacing();
    const typename InputImageType::PointType &
    inputOrigin = inputPtr->GetOrigin();
    const typename InputImageType::DirectionType &
    inputDirection = inputPtr->GetDirection();

    typename OutputImageType::SpacingType outputSpacing;
    typename OutputImageType::PointType outputOrigin;
    typename OutputImageType::DirectionType outputDirection;

    // copy the input to the output and fill the rest of the
    // output with zeros.
    for ( i = 0; i < Superclass::InputImageDimension; ++i )
      {
      outputSpacing[i] = inputSpacing[i];
      outputOrigin[i] = inputOrigin[i];
      for ( j = 0; j < Superclass::OutputImageDimension; j++ )
        {
        if ( j < Superclass::InputImageDimension )
          {
          outputDirection[j][i] = inputDirection[j][i];
          }
        else
          {
          outputDirection[j][i] = 0.0;
          }
        }
      }
    for (; i < Superclass::OutputImageDimension; ++i )
      {
      outputSpacing[i] = 1.0;
      outputOrigin[i] = 0.0;
      for ( j = 0; j < Superclass::OutputImageDimension; j++ )
        {
        if ( j == i )
          {
          outputDirection[j][i] = 1.0;
          }
        else
          {
          outputDirection[j][i] = 0.0;
          }
        }
      }

    // set the spacing and origin
    outputPtr->SetSpacing(outputSpacing);
    outputPtr->SetOrigin(outputOrigin);
    outputPtr->SetDirection(outputDirection);
    outputPtr->SetNumberOfComponentsPerPixel(  // propagate vector length info
                                                                              inputPtr->GetNumberOfComponentsPerPixel() );
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::UnaryFunctorImageFilter::GenerateOutputInformation "
                       << "cannot cast input to "
                       << typeid( ImageBase< Superclass::InputImageDimension > * ).name() );
    }
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

  const size_t numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / regionSize[0];
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
