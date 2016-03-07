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
#ifndef itkComposeImageFilter_hxx
#define itkComposeImageFilter_hxx

#include "itkComposeImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
ComposeImageFilter< TInputImage, TOutputImage >
::ComposeImageFilter()
{
  OutputPixelType p;
  int nbOfComponents = NumericTraits<OutputPixelType>::GetLength(p);
  nbOfComponents = std::max( 1, nbOfComponents );  // require at least one input
  this->SetNumberOfRequiredInputs( nbOfComponents );
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComposeImageFilter< TInputImage, TOutputImage >
::SetInput1(const InputImageType *image1)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput( 0, const_cast< InputImageType * >( image1 ) );
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComposeImageFilter< TInputImage, TOutputImage >
::SetInput2(const InputImageType *image2)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput( 1, const_cast< InputImageType * >( image2 ) );
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComposeImageFilter< TInputImage, TOutputImage >
::SetInput3(const InputImageType *image3)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput( 2, const_cast< InputImageType * >( image3 ) );
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComposeImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation(void)
{
  // Override the method in itkImageSource, so we can set the vector length of
  // the output itk::VectorImage

  this->Superclass::GenerateOutputInformation();

  OutputImageType *output = this->GetOutput();
  output->SetNumberOfComponentsPerPixel( static_cast<unsigned int>( this->GetNumberOfIndexedInputs() ) );
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComposeImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // Check to verify all inputs are specified and have the same metadata,
  // spacing etc...
  const unsigned int numberOfInputs = static_cast<const unsigned int>( this->GetNumberOfIndexedInputs() );
  RegionType         region;

  for ( unsigned int i = 0; i < numberOfInputs; i++ )
    {
    InputImageType *input = itkDynamicCastInDebugMode< InputImageType * >
      (this->ProcessObject::GetInput(i) );
    if ( !input )
      {
      itkExceptionMacro(<< "Input " << i << " not set!");
      }
    if ( i == 0 )
      {
      region = input->GetLargestPossibleRegion();
      }
    else if ( input->GetLargestPossibleRegion() != region )
      {
      itkExceptionMacro(<< "All Inputs must have the same dimensions.");
      }
    }
}

//----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
ComposeImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  typename OutputImageType::Pointer outputImage =
    static_cast< OutputImageType * >( this->ProcessObject::GetOutput(0) );

  ImageRegionIterator< OutputImageType > oit(outputImage, outputRegionForThread);
  oit.GoToBegin();

  InputIteratorContainerType inputItContainer;

  for ( unsigned int i = 0; i < this->GetNumberOfIndexedInputs(); i++ )
    {
    const InputImageType * inputImage = this->GetInput(i);

    InputIteratorType iit( inputImage, outputRegionForThread);
    iit.GoToBegin();
    inputItContainer.push_back(iit);
    }

  OutputPixelType pix;
  NumericTraits<OutputPixelType>::SetLength( pix, static_cast<unsigned int>(this->GetNumberOfIndexedInputs() ) );
  while ( !oit.IsAtEnd() )
    {
    ComputeOutputPixel( pix, inputItContainer );
    oit.Set(pix);
    ++oit;
    progress.CompletedPixel();
    }
}
} // end namespace itk

#endif
