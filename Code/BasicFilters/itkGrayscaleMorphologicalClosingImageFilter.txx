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
  :m_Kernel()
{
  m_MorphologicalClosingBoundaryCondition.SetConstant( NumericTraits<PixelType>::NonpositiveMin() );
  this->OverrideBoundaryCondition( &m_MorphologicalClosingBoundaryCondition );
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
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
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
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId) 
{
  this->AllocateOutputs();
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  /** set up erosion and dilation methods */
  typename GrayscaleDilateImageFilter<TInputImage, TOutputImage, TKernel>::Pointer
    dilate = GrayscaleDilateImageFilter<TInputImage, TOutputImage, TKernel>::New();

  typename GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::Pointer
    erode = GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::New();

  dilate->SetKernel( m_Kernel );
  erode->SetKernel( m_Kernel );
  erode->ReleaseDataFlagOn();
  
  /** set up the minipipeline */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, 1.0f);
  progress->RegisterInternalFilter(dilate, 1.0f);
  
  dilate->SetInput( this->GetInput() );
  erode->SetInput(  dilate->GetOutput() );
  erode->GraftOutput( output );

  /** execute the minipipeline */
  erode->Update();

  /** graft the minipipeline output back into this filter's output */
  this->GraftOutput( output );
}

template<class TInputImage, class TOutputImage, class TKernel>
typename GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>::PixelType
GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(const NeighborhoodIteratorType &nit,
           const KernelIteratorType kernelBegin,
           const KernelIteratorType kernelEnd)
{
  PixelType max = NumericTraits<PixelType>::NonpositiveMin();
  return max ;
}


template<class TInputImage, class TOutputImage, class TKernel>
void
GrayscaleMorphologicalClosingImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Kernel   = " << m_Kernel << std::endl;
}


}// end namespace itk
#endif
