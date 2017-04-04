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
#ifndef itkRobustAutomaticThresholdImageFilter_hxx
#define itkRobustAutomaticThresholdImageFilter_hxx

#include "itkRobustAutomaticThresholdImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk
{
template< typename TInputImage, typename TGradientImage, typename TOutputImage >
RobustAutomaticThresholdImageFilter< TInputImage, TGradientImage, TOutputImage >
::RobustAutomaticThresholdImageFilter() :
  m_Pow( 1 ),
  m_Threshold( NumericTraits< InputPixelType >::ZeroValue() ),
  m_InsideValue( NumericTraits< OutputPixelType >::max() ),
  m_OutsideValue( NumericTraits< OutputPixelType >::ZeroValue() )
{
  this->SetNumberOfRequiredInputs(2);
}

template< typename TInputImage, typename TGradientImage, typename TOutputImage >
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

  progress->RegisterInternalFilter( threshold, 1 );
  threshold->GraftOutput( this->GetOutput() );
  threshold->SetInput( this->GetInput() );
  threshold->SetLowerThreshold( m_Threshold );
  threshold->SetInsideValue( m_InsideValue );
  threshold->SetOutsideValue( m_OutsideValue );
  threshold->Update();

  this->GraftOutput( threshold->GetOutput() );
}

template< typename TInputImage, typename TGradientImage, typename TOutputImage >
void
RobustAutomaticThresholdImageFilter< TInputImage, TGradientImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  const_cast< TInputImage * >( this->GetInput() )->SetRequestedRegionToLargestPossibleRegion();
  const_cast< TGradientImage * >( this->GetGradientImage() )->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TGradientImage, typename TOutputImage >
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
