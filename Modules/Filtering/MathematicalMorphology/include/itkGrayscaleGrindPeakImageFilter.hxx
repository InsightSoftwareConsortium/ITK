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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkGrayscaleGrindPeakImageFilter_hxx
#define itkGrayscaleGrindPeakImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkGrayscaleGrindPeakImageFilter.h"
#include "itkReconstructionByDilationImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
GrayscaleGrindPeakImageFilter< TInputImage, TOutputImage >
::GrayscaleGrindPeakImageFilter():
  m_NumberOfIterationsUsed(1)
{
  m_FullyConnected = false;
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleGrindPeakImageFilter< TInputImage, TOutputImage >
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
GrayscaleGrindPeakImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleGrindPeakImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  // construct a marker image to manipulate using reconstruction by
  // dilation. the marker image will have the same pixel values as the
  // input image on the boundary of the image and will have the
  // minimum pixel value from the input image for all the pixels in
  // the interior
  //

  // compute the minimum pixel value in the input
  typename MinimumMaximumImageCalculator< TInputImage >::Pointer calculator =
    MinimumMaximumImageCalculator< TInputImage >::New();
  calculator->SetImage( this->GetInput() );
  calculator->ComputeMinimum();

  InputImagePixelType minValue;
  minValue = calculator->GetMinimum();

  // allocate a marker image
  InputImagePointer markerPtr = InputImageType::New();
  markerPtr->SetRegions( this->GetInput()->GetRequestedRegion() );
  markerPtr->CopyInformation( this->GetInput() );
  markerPtr->Allocate();

  // fill the marker image with the maximum value from the input
  markerPtr->FillBuffer(minValue);

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
  dilate->SetMaskImage( this->GetInput() );
  dilate->SetFullyConnected(m_FullyConnected);

  // graft our output to the dilate filter to force the proper regions
  // to be generated
  dilate->GraftOutput( this->GetOutput() );

  // reconstruction by dilation
  dilate->Update();

  // graft the output of the dilate filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( dilate->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
GrayscaleGrindPeakImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
} // end namespace itk
#endif
