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
#ifndef itkOtsuMultipleThresholdsImageFilter_hxx
#define itkOtsuMultipleThresholdsImageFilter_hxx

#include "itkOtsuMultipleThresholdsImageFilter.h"
#include "itkThresholdLabelerImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
OtsuMultipleThresholdsImageFilter< TInputImage, TOutputImage >
::OtsuMultipleThresholdsImageFilter() :
  m_NumberOfHistogramBins( 128 ),
  m_NumberOfThresholds( 1 ),
  m_LabelOffset( NumericTraits< OutputPixelType >::ZeroValue() ),
  m_ValleyEmphasis( false )
{
  m_Thresholds.clear();
}

template< typename TInputImage, typename TOutputImage >
void
OtsuMultipleThresholdsImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Create a histogram of the image intensities
  typename HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();
  histogramGenerator->SetInput( this->GetInput() );
  histogramGenerator->SetNumberOfBins(m_NumberOfHistogramBins);
  histogramGenerator->Compute();

  // Compute the multiple Otsu Thresholds for the input image
  typename OtsuCalculatorType::Pointer otsuHistogramThresholdCalculator = OtsuCalculatorType::New();
  otsuHistogramThresholdCalculator->SetInputHistogram( histogramGenerator->GetOutput() );
  otsuHistogramThresholdCalculator->SetNumberOfThresholds(m_NumberOfThresholds);
  otsuHistogramThresholdCalculator->SetValleyEmphasis(m_ValleyEmphasis);
  otsuHistogramThresholdCalculator->Compute();

  m_Thresholds = otsuHistogramThresholdCalculator->GetOutput();

  typename ThresholdLabelerImageFilter< TInputImage, TOutputImage >::Pointer threshold =
    ThresholdLabelerImageFilter< TInputImage, TOutputImage >::New();

  progress->RegisterInternalFilter(threshold, 1.0f);
  threshold->GraftOutput ( this->GetOutput() );
  threshold->SetInput ( this->GetInput() );
  threshold->SetRealThresholds(m_Thresholds);
  threshold->SetLabelOffset(m_LabelOffset);
  threshold->Update();

  this->GraftOutput( threshold->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
OtsuMultipleThresholdsImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  TInputImage *input = const_cast< TInputImage * >( this->GetInput() );

  if( input )
    {
    input->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
OtsuMultipleThresholdsImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfHistogramBins: " << m_NumberOfHistogramBins << std::endl;
  os << indent << "NumberOfThresholds: " << m_NumberOfThresholds << std::endl;
  os << indent << "LabelOffset: " << m_LabelOffset << std::endl;
  os << indent << "Thresholds: " << std::endl;
  for( SizeValueType j = 0; j < m_Thresholds.size(); j++ )
    {
    os << "\tThreshold #" << j << ": "
       << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Thresholds[j] )
       << std::endl;
    }
}
} // end namespace itk
#endif
