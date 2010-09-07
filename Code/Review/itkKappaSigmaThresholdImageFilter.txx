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

namespace itk
{
template< class TInputImage, class TMaskImage, class TOutputImage >
KappaSigmaThresholdImageFilter< TInputImage, TMaskImage, TOutputImage >
::KappaSigmaThresholdImageFilter()
{
  m_OutsideValue   = NumericTraits< OutputPixelType >::Zero;
  m_InsideValue    = NumericTraits< OutputPixelType >::max();
  m_Threshold      = NumericTraits< InputPixelType >::Zero;
  m_SigmaFactor = 2;
  m_NumberOfIterations = 2;
  m_MaskValue = NumericTraits< MaskPixelType >::max();
}

template< class TInputImage, class TMaskImage, class TOutputImage >
void
KappaSigmaThresholdImageFilter< TInputImage, TMaskImage, TOutputImage >
::GenerateData()
{
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Compute the Threshold for the input image
  typename CalculatorType::Pointer thresholdImageCalculator = CalculatorType::New();
  thresholdImageCalculator->SetImage( this->GetInput() );
  thresholdImageCalculator->SetMask( this->GetMaskImage() );
  thresholdImageCalculator->SetMaskValue(m_MaskValue);
  thresholdImageCalculator->SetSigmaFactor(m_SigmaFactor);
  thresholdImageCalculator->SetNumberOfIterations(m_NumberOfIterations);
  thresholdImageCalculator->Compute();

  m_Threshold = thresholdImageCalculator->GetOutput();

  typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::Pointer threshold =
    BinaryThresholdImageFilter< TInputImage, TOutputImage >::New();

  progress->RegisterInternalFilter(threshold, .5f);
  threshold->GraftOutput ( this->GetOutput() );
  threshold->SetInput ( this->GetInput() );
  threshold->SetLowerThreshold(m_Threshold);
  threshold->SetInsideValue (m_InsideValue);
  threshold->SetOutsideValue (m_OutsideValue);
  threshold->Update();

  this->GraftOutput( threshold->GetOutput() );
}

template< class TInputImage, class TMaskImage, class TOutputImage >
void
KappaSigmaThresholdImageFilter< TInputImage, TMaskImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  const_cast< TInputImage * >( this->GetInput() )->SetRequestedRegionToLargestPossibleRegion();
}

template< class TInputImage, class TMaskImage, class TOutputImage >
void
KappaSigmaThresholdImageFilter< TInputImage, TMaskImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold: " << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Threshold )
     << std::endl;
  os << indent << "MaskValue: " << static_cast< typename NumericTraits< MaskPixelType >::PrintType >( m_MaskValue )
     << std::endl;
  os << indent << "SigmaFactor: " << m_SigmaFactor << std::endl;
  os << indent << "NumberOfIterations: " << this->m_NumberOfIterations << std::endl;
  os << indent << "Inside value: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( this->m_InsideValue ) << std::endl;
  os << indent << "Outside value: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( this->m_OutsideValue ) << std::endl;
}
} // end namespace itk
#endif
