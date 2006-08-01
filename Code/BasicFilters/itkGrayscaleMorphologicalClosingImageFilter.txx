/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleMorphologicalClosingImageFilter.txx
  Language:  C++

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleMorphologicalClosingImageFilter_txx
#define __itkGrayscaleMorphologicalClosingImageFilter_txx

#include "itkGrayscaleMorphologicalClosingImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>
::GrayscaleMorphologicalClosingImageFilter()
  : m_Kernel()
{
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>
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
GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template<class TInputImage, class TOutputImage, class TKernel>
void
GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();

  /** set up erosion and dilation methods */
  typename GrayscaleDilateImageFilter<TInputImage, TOutputImage, TKernel>::Pointer
    dilate = GrayscaleDilateImageFilter<TInputImage, TOutputImage, TKernel>::New();

  typename GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::Pointer
    erode = GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::New();

  dilate->SetKernel( this->GetKernel() );
  dilate->ReleaseDataFlagOn();
  erode->SetKernel( this->GetKernel() );
  erode->ReleaseDataFlagOn();
  
  /** set up the minipipeline */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, .5f);
  progress->RegisterInternalFilter(dilate, .5f);
  
  dilate->SetInput( this->GetInput() );
  erode->SetInput(  dilate->GetOutput() );
  erode->GraftOutput( this->GetOutput() );

  /** execute the minipipeline */
  erode->Update();

  /** graft the minipipeline output back into this filter's output */
  this->GraftOutput( erode->GetOutput() );
}

template<class TInputImage, class TOutputImage, class TKernel>
void
GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
}

}// end namespace itk
#endif
