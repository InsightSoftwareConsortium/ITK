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
#ifndef itkCastImageFilter_hxx
#define itkCastImageFilter_hxx

#include "itkCastImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageAlgorithm.h"

namespace itk
{


template< typename TInputImage, typename TOutputImage >
CastImageFilter< TInputImage, TOutputImage >
::CastImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->InPlaceOff();
}

template< typename TInputImage, typename TOutputImage >
void
CastImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  if ( this->GetInPlace() && this->CanRunInPlace() )
    {
    // nothing to do, so avoid iterating over all the pixels
    // for nothing! Allocate the output, generate a fake progress and exit
    this->AllocateOutputs();
    ProgressReporter progress(this, 0, 1);
    return;
    }
  Superclass::GenerateData();
}


template< typename TInputImage, typename TOutputImage >
void
CastImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
  {
    // do not call the superclass' implementation of this method since
    // this filter allows the input the output to be of different dimensions

    // get pointers to the input and output
    TOutputImage *outputPtr = this->GetOutput();
    const TInputImage *inputPtr  = this->GetInput();

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


template< typename TInputImage, typename TOutputImage >
void
CastImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const TInputImage *inputPtr = this->GetInput();
  TOutputImage *outputPtr = this->GetOutput(0);

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  typename TInputImage::RegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // when progress is destroyed it'll set progress to 1.0
  ProgressReporter progress(this, threadId, 1);

  ImageAlgorithm::Copy( inputPtr, outputPtr, inputRegionForThread, outputRegionForThread );
}


} // end namespace itk

#endif
