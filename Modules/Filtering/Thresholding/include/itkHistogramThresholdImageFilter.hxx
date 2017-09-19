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
#ifndef itkHistogramThresholdImageFilter_hxx
#define itkHistogramThresholdImageFilter_hxx
#include "itkHistogramThresholdImageFilter.h"

#include "itkImageToHistogramFilter.h"
#include "itkMaskedImageToHistogramFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkMaskImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template<typename TInputImage, typename TOutputImage, typename TMaskImage>
HistogramThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>
::HistogramThresholdImageFilter() :
  m_InsideValue( NumericTraits<OutputPixelType>::max() ),
  m_OutsideValue( NumericTraits<OutputPixelType>::ZeroValue() ),
  m_Threshold( NumericTraits<InputPixelType>::ZeroValue() ),
  m_MaskValue ( NumericTraits<MaskPixelType>::max() ),
  m_NumberOfHistogramBins( 256 ),
  m_MaskOutput( true )
{
  this->SetNumberOfRequiredOutputs(1);

  // implicit:
  // #0 "Primary" required

  // #1 "MaskImage" optional
  Self::AddOptionalInputName("MaskImage",1);

  if( typeid(ValueType) == typeid(signed char)
      || typeid(ValueType) == typeid(unsigned char)
      || typeid(ValueType) == typeid(char))
    {
    m_AutoMinimumMaximum = false;
    }
  else
    {
    m_AutoMinimumMaximum = true;
    }
}

template<typename TInputImage, typename TOutputImage, typename TMaskImage>
void
HistogramThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>
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

  typedef itk::Statistics::MaskedImageToHistogramFilter<InputImageType, MaskImageType> MaskedHistogramGeneratorType;
  typename MaskedHistogramGeneratorType::Pointer maskedhistogramGenerator = MaskedHistogramGeneratorType::New();

  if (this->GetMaskImage())
    {
    maskedhistogramGenerator->SetInput( this->GetInput() );
    maskedhistogramGenerator->SetMaskImage(this->GetMaskImage());
    maskedhistogramGenerator->SetNumberOfThreads( this->GetNumberOfThreads() );
    typename HistogramType::SizeType hsize(this->GetInput()->GetNumberOfComponentsPerPixel());
    hsize.Fill(this->GetNumberOfHistogramBins());
    maskedhistogramGenerator->SetHistogramSize(hsize);
    maskedhistogramGenerator->SetAutoMinimumMaximum(this->GetAutoMinimumMaximum());
    maskedhistogramGenerator->SetMaskValue(this->GetMaskValue());
    progress->RegisterInternalFilter(maskedhistogramGenerator,.4f);

    m_Calculator->SetInput( maskedhistogramGenerator->GetOutput() );
    m_Calculator->SetNumberOfThreads( this->GetNumberOfThreads() );
    }
  else
    {
    histogramGenerator->SetInput( this->GetInput() );
    histogramGenerator->SetNumberOfThreads( this->GetNumberOfThreads() );
    typename HistogramType::SizeType hsize(this->GetInput()->GetNumberOfComponentsPerPixel());
    hsize.Fill(this->GetNumberOfHistogramBins());
    histogramGenerator->SetHistogramSize(hsize);
    histogramGenerator->SetAutoMinimumMaximum(this->GetAutoMinimumMaximum());
    progress->RegisterInternalFilter(histogramGenerator,.4f);

    m_Calculator->SetInput( histogramGenerator->GetOutput() );
    m_Calculator->SetNumberOfThreads( this->GetNumberOfThreads() );
    }
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

  typedef MaskImageFilter<TOutputImage, TMaskImage> MaskType;
  typename MaskType::Pointer masker = MaskType::New();

  if ((this->GetMaskOutput()) && (this->GetMaskImage()))
    {
    masker->SetInput(thresholder->GetOutput());
    masker->SetInput2(this->GetMaskImage());
    masker->SetNumberOfThreads( this->GetNumberOfThreads() );
    progress->RegisterInternalFilter(masker, .4f);
    masker->GraftOutput( this->GetOutput() );
    masker->Update();
    this->GraftOutput( masker->GetOutput() );
    }
  else
    {
    thresholder->GraftOutput( this->GetOutput() );
    thresholder->Update();
    this->GraftOutput( thresholder->GetOutput() );
    }
  m_Threshold = m_Calculator->GetThreshold();
  m_Calculator->SetInput( ITK_NULLPTR );
}

template<typename TInputImage, typename TOutputImage, typename TMaskImage>
void
HistogramThresholdImageFilter<TInputImage, TOutputImage,TMaskImage>
::GenerateInputRequestedRegion()
{
  TInputImage * input = const_cast<TInputImage *>(this->GetInput());
  if( input )
    {
    input->SetRequestedRegionToLargestPossibleRegion();
    }
}

template<typename TInputImage, typename TOutputImage, typename TMaskImage>
void
HistogramThresholdImageFilter<TInputImage,TOutputImage,TMaskImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutsideValue: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutsideValue) << std::endl;
  os << indent << "InsideValue: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_InsideValue) << std::endl;
  os << indent << "Threshold (computed): "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_Threshold) << std::endl;
  os << indent << "MaskValue: "
    << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_MaskValue) << std::endl;
  itkPrintSelfObjectMacro( Calculator );
  os << indent << "NumberOfHistogramBins: " << m_NumberOfHistogramBins << std::endl;
  os << indent << "AutoMinimumMaximm: " << m_AutoMinimumMaximum << std::endl;
  os << indent << "MaskOutput: " << m_MaskOutput << std::endl;
}
}// end namespace itk
#endif
