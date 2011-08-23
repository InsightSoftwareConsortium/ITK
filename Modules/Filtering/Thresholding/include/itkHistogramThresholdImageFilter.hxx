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
#ifndef __itkHistogramThresholdImageFilter_hxx
#define __itkHistogramThresholdImageFilter_hxx
#include "itkHistogramThresholdImageFilter.h"

#include "itkImageToHistogramFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template<class TInputImage, class TOutputImage>
HistogramThresholdImageFilter<TInputImage, TOutputImage>
::HistogramThresholdImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  m_OutsideValue   = NumericTraits<OutputPixelType>::Zero;
  m_InsideValue    = NumericTraits<OutputPixelType>::max();
  m_Threshold      = NumericTraits<InputPixelType>::Zero;
  m_Calculator     = NULL;

  if( typeid(ValueType) == typeid(signed char) || typeid(ValueType) == typeid(unsigned char) )
    {
    this->SetAutoMinimumMaximum(false);
    }
  else
    {
    this->SetAutoMinimumMaximum(true);
    }

  m_NumberOfHistogramBins = 256;

}


template<class TInputImage, class TOutputImage>
void
HistogramThresholdImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  if( m_Calculator.IsNull() )
    {
    itkExceptionMacro(<<"No threshold calculator set.");
    }
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  typedef itk::Statistics::ImageToHistogramFilter<InputImageType> HistogramGeneratorType;
  typename HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();
  histogramGenerator->SetInput( this->GetInput() );

  histogramGenerator->SetNumberOfThreads( this->GetNumberOfThreads() );
  typename HistogramType::SizeType hsize(this->GetInput()->GetNumberOfComponentsPerPixel());
  hsize.Fill(this->GetNumberOfHistogramBins());
  histogramGenerator->SetHistogramSize(hsize);
  histogramGenerator->SetAutoMinimumMaximum(this->GetAutoMinimumMaximum());
  progress->RegisterInternalFilter(histogramGenerator,.4f);

  m_Calculator->SetInput( histogramGenerator->GetOutput() );
  m_Calculator->SetNumberOfThreads( this->GetNumberOfThreads() );

  progress->RegisterInternalFilter(m_Calculator,.2f);

  typedef BinaryThresholdImageFilter<TInputImage,TOutputImage> ThresholderType;
  typename ThresholderType::Pointer thresholder = ThresholderType::New();
  thresholder->SetInput(this->GetInput());
  thresholder->SetLowerThreshold( NumericTraits<InputPixelType>::NonpositiveMin() );
  thresholder->SetUpperThresholdInput( m_Calculator->GetOutput() );
  thresholder->SetInsideValue( this->GetInsideValue() );
  thresholder->SetOutsideValue( this->GetOutsideValue() );
  thresholder->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(thresholder,.4f);

  thresholder->GraftOutput( this->GetOutput() );
  thresholder->Update();
  this->GraftOutput( thresholder->GetOutput() );
  m_Threshold = m_Calculator->GetThreshold();
  m_Calculator->SetInput( NULL );
}

template<class TInputImage, class TOutputImage>
void
HistogramThresholdImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  TInputImage * input = const_cast<TInputImage *>(this->GetInput());
  if( input )
    {
    input->SetRequestedRegionToLargestPossibleRegion();
    }
}

template<class TInputImage, class TOutputImage>
void
HistogramThresholdImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "OutsideValue: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutsideValue) << std::endl;
  os << indent << "InsideValue: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_InsideValue) << std::endl;
  os << indent << "Calculator: ";
  m_Calculator->Print( os, indent.GetNextIndent() );
  os << indent << "Threshold (computed): "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_Threshold) << std::endl;
}


}// end namespace itk
#endif
