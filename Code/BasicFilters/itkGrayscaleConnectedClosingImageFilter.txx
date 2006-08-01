/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleConnectedClosingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleConnectedClosingImageFilter_txx
#define __itkGrayscaleConnectedClosingImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkGrayscaleConnectedClosingImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <class TInputImage, class TOutputImage>
GrayscaleConnectedClosingImageFilter<TInputImage, TOutputImage>
::GrayscaleConnectedClosingImageFilter()
  : m_NumberOfIterationsUsed( 1 )
{
  m_Seed.Fill( NumericTraits<ITK_TYPENAME InputImageIndexType::OffsetValueType>::Zero );
  m_FullyConnected = false;
}

template <class TInputImage, class TOutputImage>
void 
GrayscaleConnectedClosingImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}


template <class TInputImage, class TOutputImage>
void 
GrayscaleConnectedClosingImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
GrayscaleConnectedClosingImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  
  // construct a marker image to manipulate using reconstruction by
  // dilation. the marker image will have the same pixel values as the
  // input image at the seed pixel and will have a minimum everywhere
  // else. 
  //

  // compute the minimum pixel value in the input
  typename MinimumMaximumImageCalculator<TInputImage>::Pointer calculator
    = MinimumMaximumImageCalculator<TInputImage>::New();
  calculator->SetImage( this->GetInput() );
  calculator->ComputeMaximum();

  InputImagePixelType maxValue;
  maxValue = calculator->GetMaximum();

  // compare this maximum value to the value at the seed pixel.
  InputImagePixelType seedValue;
  seedValue = this->GetInput()->GetPixel( m_Seed );

  if (maxValue == seedValue)
    {
    itkWarningMacro(<< "GrayscaleConnectedClosingImageFilter: pixel value at seed point matches maximum value in image.  Resulting image will have a constant value.");
    this->GetOutput()->FillBuffer( maxValue );
    this->UpdateProgress(1.0);
    return;
    }
  
  // allocate a marker image
  InputImagePointer markerPtr = InputImageType::New();
  markerPtr->SetRequestedRegion( this->GetInput()->GetRequestedRegion() );
  markerPtr->SetBufferedRegion( this->GetInput()->GetBufferedRegion() );
  markerPtr
    ->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  markerPtr->Allocate();

  // fill the marker image with the maximum value from the input
  markerPtr->FillBuffer( maxValue );

  // mark the seed point
  markerPtr->SetPixel( m_Seed, seedValue );
    
  // Delegate to a geodesic dilation filter.
  //
  //
  typename ReconstructionByErosionImageFilter<TInputImage, TInputImage>::Pointer
    erode
    = ReconstructionByErosionImageFilter<TInputImage, TInputImage>::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode,1.0f);

  // set up the erode filter
  //erode->RunOneIterationOff();             // run to convergence
  erode->SetMarkerImage( markerPtr );
  erode->SetMaskImage( this->GetInput() );
  erode->SetFullyConnected( m_FullyConnected );

  // graft our output to the erode filter to force the proper regions
  // to be generated
  erode->GraftOutput( this->GetOutput() );

  // reconstruction by dilation
  erode->Update();

  // graft the output of the erode filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( erode->GetOutput() );
}


template<class TInputImage, class TOutputImage>
void
GrayscaleConnectedClosingImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Seed point: " << m_Seed << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
  
}// end namespace itk
#endif
