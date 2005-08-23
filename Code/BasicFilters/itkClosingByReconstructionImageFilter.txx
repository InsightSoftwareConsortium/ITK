/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClosingByReconstructionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkClosingByReconstructionImageFilter_txx
#define __itkClosingByReconstructionImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkClosingByReconstructionImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk {

template <class TInputImage, class TOutputImage, class TKernel>
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::ClosingByReconstructionImageFilter()
  : m_Kernel()
{
  m_FullyConnected = false;
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage, class TKernel>
void 
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage, class TKernel>
void 
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();
  
  // Delegate to a dilate filter.
  typename GrayscaleDilateImageFilter<TInputImage, TInputImage, TKernel>::Pointer
    dilate = GrayscaleDilateImageFilter<TInputImage, TInputImage, TKernel>::New();

  dilate->SetInput( this->GetInput() );
  dilate->SetKernel( this->m_Kernel );

  // Delegate to a dilate filter.
  typename ReconstructionByErosionImageFilter<TInputImage, TInputImage>::Pointer
    erode = ReconstructionByErosionImageFilter<TInputImage, TInputImage>::New();

  erode->SetMarkerImage( dilate->GetOutput() );
  erode->SetMaskImage( this->GetInput() );
  erode->SetFullyConnected( m_FullyConnected );

  // graft our output to the subtract filter to force the proper regions
  // to be generated
  erode->GraftOutput( this->GetOutput() );

  // run the algorithm
  progress->RegisterInternalFilter(dilate, 0.6f);
  progress->RegisterInternalFilter(erode, 0.4f);

  erode->Update();

  // graft the output of the dilate filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( erode->GetOutput() );

}

template<class TInputImage, class TOutputImage, class TKernel>
void
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}

}// end namespace itk
#endif
