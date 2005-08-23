/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpeningByReconstructionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOpeningByReconstructionImageFilter_txx
#define __itkOpeningByReconstructionImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkOpeningByReconstructionImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkReconstructionByDilationImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk {

template <class TInputImage, class TOutputImage, class TKernel>
OpeningByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::OpeningByReconstructionImageFilter()
  : m_Kernel()
{
  m_FullyConnected = false;
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
OpeningByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
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
OpeningByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage, class TKernel>
void 
OpeningByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();
  
  // Delegate to an erode filter.
  typename GrayscaleErodeImageFilter<TInputImage, TInputImage, TKernel>::Pointer
    erode = GrayscaleErodeImageFilter<TInputImage, TInputImage, TKernel>::New();

  erode->SetInput( this->GetInput() );
  erode->SetKernel( this->m_Kernel );

  // Delegate to a dilate filter.
  typename ReconstructionByDilationImageFilter<TInputImage, TInputImage>::Pointer
  dilate = ReconstructionByDilationImageFilter<TInputImage, TInputImage>::New();

  dilate->SetMarkerImage( erode->GetOutput() );
  dilate->SetMaskImage( this->GetInput() );
  dilate->SetFullyConnected( m_FullyConnected );

  // graft our output to the subtract filter to force the proper regions
  // to be generated
  dilate->GraftOutput( this->GetOutput() );

  // run the algorithm
  progress->RegisterInternalFilter(erode, 0.6f);
  progress->RegisterInternalFilter(dilate, 0.4f);

  dilate->Update();

  // graft the output of the dilate filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( dilate->GetOutput() );

}

template<class TInputImage, class TOutputImage, class TKernel>
void
OpeningByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}

}// end namespace itk
#endif
