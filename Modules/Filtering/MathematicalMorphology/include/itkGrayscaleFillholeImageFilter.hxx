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
#ifndef itkGrayscaleFillholeImageFilter_hxx
#define itkGrayscaleFillholeImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkGrayscaleFillholeImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
GrayscaleFillholeImageFilter< TInputImage, TOutputImage >
::GrayscaleFillholeImageFilter():
  m_NumberOfIterationsUsed(1)
{
  m_FullyConnected = false;
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleFillholeImageFilter< TInputImage, TOutputImage >
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
GrayscaleFillholeImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleFillholeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  // construct a marker image to manipulate using reconstruction by
  // erosion. the marker image will have the same pixel values as the
  // input image on the boundary of the image and will have the
  // maximum pixel value from the input image for all the pixels in
  // the interior
  //

  // compute the maximum pixel value in the input
  typename MinimumMaximumImageCalculator< TInputImage >::Pointer calculator =
    MinimumMaximumImageCalculator< TInputImage >::New();
  calculator->SetImage( this->GetInput() );
  calculator->ComputeMaximum();

  InputImagePixelType maxValue;
  maxValue = calculator->GetMaximum();

  // allocate a marker image
  InputImagePointer markerPtr = InputImageType::New();
  markerPtr->SetRegions( this->GetInput()->GetRequestedRegion() );
  markerPtr->CopyInformation( this->GetInput() );
  markerPtr->Allocate();

  // fill the marker image with the maximum value from the input
  markerPtr->FillBuffer(maxValue);

  // copy the borders of the input image to the marker image
  //
  ImageRegionExclusionConstIteratorWithIndex< TInputImage >
  inputBoundaryIt( this->GetInput(), this->GetInput()->GetRequestedRegion() );
  inputBoundaryIt.SetExclusionRegionToInsetRegion();

  ImageRegionExclusionIteratorWithIndex< TInputImage >
  markerBoundaryIt( markerPtr, this->GetInput()->GetRequestedRegion() );
  markerBoundaryIt.SetExclusionRegionToInsetRegion();

  // copy the boundary pixels
  inputBoundaryIt.GoToBegin();
  markerBoundaryIt.GoToBegin();
  while ( !inputBoundaryIt.IsAtEnd() )
    {
    markerBoundaryIt.Set( inputBoundaryIt.Get() );
    ++markerBoundaryIt;
    ++inputBoundaryIt;
    }

  // Delegate to a geodesic erosion filter.
  //
  //
  typename ReconstructionByErosionImageFilter< TInputImage, TInputImage >::Pointer
  erode =
    ReconstructionByErosionImageFilter< TInputImage, TInputImage >::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, 1.0f);

  // set up the erode filter
  //erode->RunOneIterationOff();             // run to convergence
  erode->SetMarkerImage(markerPtr);
  erode->SetMaskImage( this->GetInput() );
  erode->SetFullyConnected(m_FullyConnected);

  // graft our output to the erode filter to force the proper regions
  // to be generated
  erode->GraftOutput( this->GetOutput() );

  // reconstruction by erosion
  erode->Update();

  // graft the output of the erode filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( erode->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleFillholeImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
} // end namespace itk
#endif
