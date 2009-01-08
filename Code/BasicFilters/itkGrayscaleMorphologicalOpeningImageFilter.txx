/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleMorphologicalOpeningImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleMorphologicalOpeningImageFilter_txx
#define __itkGrayscaleMorphologicalOpeningImageFilter_txx

// First make sure that the configuration is available.
// This line can be removed once the optimized versions
// gets integrated into the main directories.
#include "itkConfigure.h"

#ifdef ITK_USE_CONSOLIDATED_MORPHOLOGY
#include "itkOptGrayscaleMorphologicalOpeningImageFilter.h"
#else


#include "itkGrayscaleMorphologicalOpeningImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkCropImageFilter.h"
#include "itkConstantPadImageFilter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
GrayscaleMorphologicalOpeningImageFilter<TInputImage, TOutputImage, TKernel>
::GrayscaleMorphologicalOpeningImageFilter()
  : m_Kernel()
{
  m_SafeBorder = true;
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
GrayscaleMorphologicalOpeningImageFilter<TInputImage, TOutputImage, TKernel>
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
GrayscaleMorphologicalOpeningImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(
                              this->GetOutput()->GetLargestPossibleRegion());
}

template<class TInputImage, class TOutputImage, class TKernel>
void
GrayscaleMorphologicalOpeningImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();
  
  /** set up erosion and dilation methods */
  typename GrayscaleDilateImageFilter<TOutputImage, TOutputImage, TKernel>::Pointer
    dilate = GrayscaleDilateImageFilter<TOutputImage, TOutputImage, TKernel>::New();

  typename GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::Pointer
    erode = GrayscaleErodeImageFilter<TInputImage, TOutputImage, TKernel>::New();

  dilate->SetKernel( this->GetKernel() );//structuringElement
  dilate->ReleaseDataFlagOn();
  erode->SetKernel( this->GetKernel() );
  dilate->SetInput( erode->GetOutput() );

  if ( m_SafeBorder )
    {
    typedef ConstantPadImageFilter<InputImageType, InputImageType> PadType;
    typename PadType::Pointer pad = PadType::New();
    pad->SetPadLowerBound( m_Kernel.GetRadius().m_Size );
    pad->SetPadUpperBound( m_Kernel.GetRadius().m_Size );
    pad->SetConstant( NumericTraits<ITK_TYPENAME InputImageType::PixelType>::max() );
    pad->SetInput( this->GetInput() );

    erode->SetInput( pad->GetOutput() );
    
    typedef CropImageFilter<TOutputImage, TOutputImage> CropType;
    typename CropType::Pointer crop = CropType::New();
    crop->SetInput( dilate->GetOutput() );
    crop->SetUpperBoundaryCropSize( m_Kernel.GetRadius() );
    crop->SetLowerBoundaryCropSize( m_Kernel.GetRadius() );
    
    ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
    progress->SetMiniPipelineFilter(this);
    progress->RegisterInternalFilter(pad, .1f);
    progress->RegisterInternalFilter(erode, .4f);
    progress->RegisterInternalFilter(dilate, .4f);
    progress->RegisterInternalFilter(crop, .1f);
    
    crop->GraftOutput( this->GetOutput() );
    /** execute the minipipeline */
    crop->Update();
  
    /** graft the minipipeline output back into this filter's output */
    this->GraftOutput( crop->GetOutput() );
    }
  else
    {
    /** set up the minipipeline */
    ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
    progress->SetMiniPipelineFilter(this);
    progress->RegisterInternalFilter(erode, .5f);
    progress->RegisterInternalFilter(dilate, .5f);
    
    erode->SetInput( this->GetInput() );
    dilate->GraftOutput( this->GetOutput() );

    /** execute the minipipeline */
    dilate->Update();

    /** graft the minipipeline output back into this filter's output */
    this->GraftOutput( this->GetOutput() );
    }
}

template<class TInputImage, class TOutputImage, class TKernel>
void
GrayscaleMorphologicalOpeningImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
  os << indent << "SafeBorder: " << m_SafeBorder << std::endl;
}

}// end namespace itk
#endif

#endif
