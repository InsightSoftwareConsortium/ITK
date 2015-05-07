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
#ifndef itkGrayscaleConnectedOpeningImageFilter_hxx
#define itkGrayscaleConnectedOpeningImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkGrayscaleConnectedOpeningImageFilter.h"
#include "itkReconstructionByDilationImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
GrayscaleConnectedOpeningImageFilter< TInputImage, TOutputImage >
::GrayscaleConnectedOpeningImageFilter():
  m_NumberOfIterationsUsed(1)
{
  m_Seed.Fill(NumericTraits< typename InputImageIndexType::OffsetValueType >::ZeroValue());
  m_FullyConnected = false;
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleConnectedOpeningImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleConnectedOpeningImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleConnectedOpeningImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();


  OutputImageType *outputImage = this->GetOutput();
  const InputImageType * inputImage = this->GetInput();

  // construct a marker image to manipulate using reconstruction by
  // dilation. the marker image will have the same pixel values as the
  // input image at the seed pixel and will have a minimum everywhere
  // else.
  //

  // compute the minimum pixel value in the input
  typename MinimumMaximumImageCalculator< TInputImage >::Pointer calculator =
    MinimumMaximumImageCalculator< TInputImage >::New();
  calculator->SetImage( inputImage );
  calculator->ComputeMinimum();

  InputImagePixelType minValue;
  minValue = calculator->GetMinimum();

  // compare this minimum value to the value at the seed pixel.
  InputImagePixelType seedValue;
  seedValue = inputImage->GetPixel(m_Seed);

  if ( minValue == seedValue )
    {
    itkWarningMacro(
      <<
      "GrayscaleConnectedClosingImageFilter: pixel value at seed point matches minimum value in image.  Resulting image will have a constant value.");
    outputImage->FillBuffer(minValue);
    return;
    }

  // allocate a marker image
  InputImagePointer markerPtr = InputImageType::New();
  markerPtr->SetRegions( inputImage->GetRequestedRegion() );
  markerPtr->CopyInformation( inputImage );
  markerPtr->Allocate();

  // fill the marker image with the maximum value from the input
  markerPtr->FillBuffer(minValue);

  // mark the seed point
  markerPtr->SetPixel(m_Seed, seedValue);

  // Delegate to a geodesic dilation filter.
  //
  //
  typename ReconstructionByDilationImageFilter< TInputImage, TInputImage >::Pointer
  dilate =
    ReconstructionByDilationImageFilter< TInputImage, TInputImage >::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(dilate, 1.0f);

  // set up the dilate filter
  //dilate->RunOneIterationOff();             // run to convergence
  dilate->SetMarkerImage(markerPtr);
  dilate->SetMaskImage( inputImage );
  dilate->SetFullyConnected(m_FullyConnected);

  // graft our output to the dilate filter to force the proper regions
  // to be generated
  dilate->GraftOutput( outputImage );

  // reconstruction by dilation
  dilate->Update();

  // graft the output of the dilate filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( dilate->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleConnectedOpeningImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Seed point: " << m_Seed << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
} // end namespace itk
#endif
