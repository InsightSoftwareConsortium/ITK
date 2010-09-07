/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRobustAutomaticThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRobustAutomaticThresholdImageFilter_txx
#define __itkRobustAutomaticThresholdImageFilter_txx

#include "itkRobustAutomaticThresholdImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TGradientImage, class TOutputImage >
RobustAutomaticThresholdImageFilter< TInputImage, TGradientImage, TOutputImage >
::RobustAutomaticThresholdImageFilter()
{
  m_OutsideValue   = NumericTraits< OutputPixelType >::Zero;
  m_InsideValue    = NumericTraits< OutputPixelType >::max();
  m_Threshold      = NumericTraits< InputPixelType >::Zero;
  m_Pow = 1;
  this->SetNumberOfRequiredInputs(2);
}

template< class TInputImage, class TGradientImage, class TOutputImage >
void
RobustAutomaticThresholdImageFilter< TInputImage, TGradientImage, TOutputImage >
::GenerateData()
{
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Compute the Threshold for the input image
  typename CalculatorType::Pointer thresholdCalculator = CalculatorType::New();
  thresholdCalculator->SetInput( this->GetInput() );
  thresholdCalculator->SetGradient( this->GetGradientImage() );
  thresholdCalculator->SetPow(m_Pow);
  thresholdCalculator->Compute();

  m_Threshold = thresholdCalculator->GetOutput();

  typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::Pointer threshold =
    BinaryThresholdImageFilter< TInputImage, TOutputImage >::New();

  progress->RegisterInternalFilter(threshold, 1);
  threshold->GraftOutput ( this->GetOutput() );
  threshold->SetInput ( this->GetInput() );
  threshold->SetLowerThreshold(m_Threshold);
  threshold->SetInsideValue (m_InsideValue);
  threshold->SetOutsideValue (m_OutsideValue);
  threshold->Update();

  this->GraftOutput( threshold->GetOutput() );
}

template< class TInputImage, class TGradientImage, class TOutputImage >
void
RobustAutomaticThresholdImageFilter< TInputImage, TGradientImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  const_cast< TInputImage * >( this->GetInput() )->SetRequestedRegionToLargestPossibleRegion();
  const_cast< TGradientImage * >( this->GetGradientImage() )->SetRequestedRegionToLargestPossibleRegion();
}

template< class TInputImage, class TGradientImage, class TOutputImage >
void
RobustAutomaticThresholdImageFilter< TInputImage, TGradientImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold: " << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Threshold )
     << std::endl;
  os << indent << "Pow: " << m_Pow << std::endl;
  os << indent << "OutsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutsideValue ) << std::endl;
  os << indent << "InsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_InsideValue ) << std::endl;
}
} // end namespace itk
#endif
