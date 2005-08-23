/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHMinimaImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHMinimaImageFilter_txx
#define __itkHMinimaImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkHMinimaImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <class TInputImage, class TOutputImage>
HMinimaImageFilter<TInputImage, TOutputImage>
::HMinimaImageFilter()
{
  m_Height =  2;
  m_NumberOfIterationsUsed = 1;
  m_FullyConnected = false;
}

template <class TInputImage, class TOutputImage>
void 
HMinimaImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage>
void 
HMinimaImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
HMinimaImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  
  // construct a marker image to manipulate using reconstruction by
  // erosion. the marker image is the input image minus the height
  // parameter.
  typedef ShiftScaleImageFilter<TInputImage, TInputImage>
    ShiftFilterType;
  typename ShiftFilterType::Pointer shift = ShiftFilterType::New();
  shift->SetInput( this->GetInput() );
  shift->SetShift( static_cast<typename ShiftFilterType::RealType>(m_Height) );

  // Delegate to a geodesic erosion filter.
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
  erode->SetMarkerImage( shift->GetOutput() );
  erode->SetMaskImage( this->GetInput() );
  erode->SetFullyConnected( m_FullyConnected );

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


template<class TInputImage, class TOutputImage>
void
HMinimaImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Depth of local maxima (contrast): "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_Height)
     << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
  
}// end namespace itk
#endif
