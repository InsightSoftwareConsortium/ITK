/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMorphologicalOpeningImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBinaryMorphologicalOpeningImageFilter_txx
#define __itkBinaryMorphologicalOpeningImageFilter_txx

#include "itkBinaryMorphologicalOpeningImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
BinaryMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::BinaryMorphologicalOpeningImageFilter()
{
  m_ForegroundValue = NumericTraits< PixelType >::max();
  m_BackgroundValue = NumericTraits< PixelType >::Zero;
}

template< class TInputImage, class TOutputImage, class TKernel >
void
BinaryMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();

  /** set up erosion and dilation methods */
  typename BinaryDilateImageFilter< TInputImage, TInputImage, TKernel >::Pointer
  dilate = BinaryDilateImageFilter< TInputImage, TInputImage, TKernel >::New();

  typename BinaryErodeImageFilter< TInputImage, TOutputImage, TKernel >::Pointer
  erode = BinaryErodeImageFilter< TInputImage, TOutputImage, TKernel >::New();

  dilate->SetKernel( this->GetKernel() );
  dilate->ReleaseDataFlagOn();
  erode->SetKernel( this->GetKernel() );
  erode->ReleaseDataFlagOn();
  dilate->SetDilateValue(m_ForegroundValue);
  erode->SetErodeValue(m_ForegroundValue);
  erode->SetBackgroundValue(m_BackgroundValue);

  /** set up the minipipeline */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, .5f);
  progress->RegisterInternalFilter(dilate, .5f);

  erode->SetInput( this->GetInput() );
  dilate->SetInput( erode->GetOutput() );
  dilate->GraftOutput( this->GetOutput() );

  /** execute the minipipeline */
  dilate->Update();

  /** graft the minipipeline output back into this filter's output */
  this->GraftOutput( dilate->GetOutput() );
}

template< class TInputImage, class TOutputImage, class TKernel >
void
BinaryMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_BackgroundValue ) << std::endl;
}
} // end namespace itk
#endif
