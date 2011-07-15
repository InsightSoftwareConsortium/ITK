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
#ifndef __itkOtsuThresholdImageFilter_hxx
#define __itkOtsuThresholdImageFilter_hxx
#include "itkOtsuThresholdImageFilter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkOtsuThresholdImageCalculator.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
OtsuThresholdImageFilter< TInputImage, TOutputImage >
::OtsuThresholdImageFilter()
{
  m_OutsideValue   = NumericTraits< OutputPixelType >::Zero;
  m_InsideValue    = NumericTraits< OutputPixelType >::max();
  m_Threshold      = NumericTraits< InputPixelType >::Zero;
  m_NumberOfHistogramBins = 128;
}

template< class TInputImage, class TOutputImage >
void
OtsuThresholdImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Compute the Otsu Threshold for the input image
  typename OtsuThresholdImageCalculator< TInputImage >::Pointer otsu =
    OtsuThresholdImageCalculator< TInputImage >::New();
  otsu->SetImage ( this->GetInput() );
  otsu->SetNumberOfHistogramBins (m_NumberOfHistogramBins);
  otsu->Compute();
  m_Threshold = otsu->GetThreshold();

  typename BinaryThresholdImageFilter< TInputImage, TOutputImage >::Pointer threshold =
    BinaryThresholdImageFilter< TInputImage, TOutputImage >::New();

  progress->RegisterInternalFilter(threshold, .5f);
  threshold->GraftOutput ( this->GetOutput() );
  threshold->SetInput ( this->GetInput() );
  threshold->SetLowerThreshold( NumericTraits< InputPixelType >::NonpositiveMin() );
  threshold->SetUpperThreshold( otsu->GetThreshold() );
  threshold->SetInsideValue (m_InsideValue);
  threshold->SetOutsideValue (m_OutsideValue);
  threshold->Update();

  this->GraftOutput( threshold->GetOutput() );
}

template< class TInputImage, class TOutputImage >
void
OtsuThresholdImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  TInputImage *input = const_cast< TInputImage * >( this->GetInput() );

  if ( input )
    {
    input->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< class TInputImage, class TOutputImage >
void
OtsuThresholdImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutsideValue ) << std::endl;
  os << indent << "InsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_InsideValue ) << std::endl;
  os << indent << "NumberOfHistogramBins: "
     << m_NumberOfHistogramBins << std::endl;
  os << indent << "Threshold (computed): "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Threshold ) << std::endl;
}
} // end namespace itk
#endif
