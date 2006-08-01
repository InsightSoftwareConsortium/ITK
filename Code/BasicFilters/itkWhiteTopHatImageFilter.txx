/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWhiteTopHatImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWhiteTopHatImageFilter_txx
#define __itkWhiteTopHatImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkWhiteTopHatImageFilter.h"
#include "itkGrayscaleMorphologicalOpeningImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk {

template <class TInputImage, class TOutputImage, class TKernel>
WhiteTopHatImageFilter<TInputImage, TOutputImage, TKernel>
::WhiteTopHatImageFilter()
  : m_Kernel()
{
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
WhiteTopHatImageFilter<TInputImage, TOutputImage, TKernel>
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


template <class TInputImage, class TOutputImage, class TKernel>
void 
WhiteTopHatImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage, class TKernel>
void 
WhiteTopHatImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();
  
  // Delegate to an opening filter.
  typename GrayscaleMorphologicalOpeningImageFilter<TInputImage, TInputImage, TKernel>::Pointer
    open = GrayscaleMorphologicalOpeningImageFilter<TInputImage, TInputImage, TKernel>::New();

  open->SetInput( this->GetInput() );
  open->SetKernel(this->m_Kernel);
  
  // Need to subtract the opened image from the input
  typename SubtractImageFilter<TInputImage, TInputImage, TOutputImage>::Pointer
    subtract=SubtractImageFilter<TInputImage,TInputImage,TOutputImage>::New();

  subtract->SetInput1( this->GetInput() );
  subtract->SetInput2( open->GetOutput() );

  // graft our output to the subtract filter to force the proper regions
  // to be generated
  subtract->GraftOutput( this->GetOutput() );

  // run the algorithm
  progress->RegisterInternalFilter(open,.9f);
  progress->RegisterInternalFilter(subtract,.1f);

  subtract->Update();

  // graft the output of the subtract filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( subtract->GetOutput() );

}

template<class TInputImage, class TOutputImage, class TKernel>
void
WhiteTopHatImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
}

}// end namespace itk
#endif
