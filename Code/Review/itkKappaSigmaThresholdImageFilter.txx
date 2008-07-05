/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKappaSigmaThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKappaSigmaThresholdImageFilter_txx
#define __itkKappaSigmaThresholdImageFilter_txx

#include "itkKappaSigmaThresholdImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template<class TInputImage, class TMaskImage, class TOutputImage>
KappaSigmaThresholdImageFilter<TInputImage, TMaskImage, TOutputImage>
::KappaSigmaThresholdImageFilter()
{
  m_OutsideValue   = NumericTraits<OutputPixelType>::Zero;
  m_InsideValue    = NumericTraits<OutputPixelType>::max();
  m_Threshold      = NumericTraits<InputPixelType>::Zero;
  m_SigmaFactor = 2;
  m_NumberOfIterations = 2;
  m_MaskValue = NumericTraits<MaskPixelType>::max();
}

template<class TInputImage, class TMaskImage, class TOutputImage>
void
KappaSigmaThresholdImageFilter<TInputImage, TMaskImage, TOutputImage>
::GenerateData()
{
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Compute the Threshold for the input image
  typename CalculatorType::Pointer thresholdCalculator = CalculatorType::New();
  thresholdCalculator->SetInput( this->GetInput() );
  thresholdCalculator->SetMask( this->GetMaskImage() );
  thresholdCalculator->SetMaskValue( m_MaskValue );
  thresholdCalculator->SetSigmaFactor( m_SigmaFactor );
  thresholdCalculator->SetNumberOfIterations( m_NumberOfIterations );
  thresholdCalculator->Compute();

  m_Threshold = thresholdCalculator->GetOutput();

  typename BinaryThresholdImageFilter<TInputImage,TOutputImage>::Pointer threshold = 
    BinaryThresholdImageFilter<TInputImage,TOutputImage>::New();
  
  progress->RegisterInternalFilter(threshold,.5f);
  threshold->GraftOutput (this->GetOutput());
  threshold->SetInput (this->GetInput());
  threshold->SetLowerThreshold( m_Threshold );
  threshold->SetInsideValue (m_InsideValue);
  threshold->SetOutsideValue (m_OutsideValue);
  threshold->Update();

  this->GraftOutput(threshold->GetOutput());
}

template<class TInputImage, class TMaskImage, class TOutputImage>
void
KappaSigmaThresholdImageFilter<TInputImage, TMaskImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  const_cast<TInputImage *>(this->GetInput())->SetRequestedRegionToLargestPossibleRegion();
}

template<class TInputImage, class TMaskImage, class TOutputImage>
void 
KappaSigmaThresholdImageFilter<TInputImage, TMaskImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Threshold: " << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_Threshold) << std::endl;
  os << indent << "MaskValue: " << static_cast<typename NumericTraits<MaskPixelType>::PrintType>(m_MaskValue) << std::endl;
  os << indent << "SigmaFactor: " << m_SigmaFactor << std::endl;
  os << indent << "NumberOfIterations: " << m_NumberOfIterations << std::endl;
}


}// end namespace itk
#endif
