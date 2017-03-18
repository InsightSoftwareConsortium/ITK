/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkKappaSigmaThresholdImageFilter_hxx
#define itkKappaSigmaThresholdImageFilter_hxx

#include "itkKappaSigmaThresholdImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TMaskImage, typename TOutputImage >
KappaSigmaThresholdImageFilter< TInputImage, TMaskImage, TOutputImage >
::KappaSigmaThresholdImageFilter() :
  m_MaskValue( NumericTraits<MaskPixelType>::max() ),
  m_SigmaFactor( 2 ),
  m_NumberOfIterations( 2 ),
  m_Threshold( NumericTraits<InputPixelType>::ZeroValue() ),
  m_InsideValue( NumericTraits<OutputPixelType>::max() ),
  m_OutsideValue( NumericTraits<OutputPixelType>::ZeroValue() )
{
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
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

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
void
KappaSigmaThresholdImageFilter< TInputImage, TMaskImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  const_cast< TInputImage * >( this->GetInput() )->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
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
