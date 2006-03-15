/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOtsuMultipleThresholdsImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkOtsuMultipleThresholdsImageFilter_txx
#define _itkOtsuMultipleThresholdsImageFilter_txx

#include "itkOtsuMultipleThresholdsImageFilter.h"
#include "itkThresholdLabelerImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkScalarImageToHistogramGenerator.h"
#include "itkOtsuMultipleThresholdsCalculator.h"

namespace itk {

template<class TInputImage, class TOutputImage>
OtsuMultipleThresholdsImageFilter<TInputImage, TOutputImage>
::OtsuMultipleThresholdsImageFilter()
{
  m_NumberOfHistogramBins = 128;
  m_NumberOfThresholds = 1;
  m_LabelOffset = NumericTraits<OutputPixelType>::Zero;
  m_Thresholds.clear();
}

template<class TInputImage, class TOutputImage>
void
OtsuMultipleThresholdsImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Create a histogram of the image intensities
  typename HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();
  histogramGenerator->SetInput(  this->GetInput()  );
  histogramGenerator->SetNumberOfBins( m_NumberOfHistogramBins );
  histogramGenerator->Compute();

  // Compute the multiple Otsu Thresholds for the input image
  typename OtsuCalculatorType::Pointer otsuThresholdCalculator = OtsuCalculatorType::New();
  otsuThresholdCalculator->SetInputHistogram( histogramGenerator->GetOutput() );
  otsuThresholdCalculator->SetNumberOfThresholds( m_NumberOfThresholds );
  otsuThresholdCalculator->Update();

  m_Thresholds = otsuThresholdCalculator->GetOutput();

  typename ThresholdLabelerImageFilter<TInputImage,TOutputImage>::Pointer threshold = 
    ThresholdLabelerImageFilter<TInputImage,TOutputImage>::New();;
  
  progress->RegisterInternalFilter(threshold,.5f);
  threshold->GraftOutput (this->GetOutput());
  threshold->SetInput (this->GetInput());
  threshold->SetRealThresholds( m_Thresholds );
  threshold->SetLabelOffset( m_LabelOffset );
  threshold->Update();

  this->GraftOutput(threshold->GetOutput());
}

template<class TInputImage, class TOutputImage>
void
OtsuMultipleThresholdsImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  const_cast<TInputImage *>(this->GetInput())->SetRequestedRegionToLargestPossibleRegion();
}

template<class TInputImage, class TOutputImage>
void 
OtsuMultipleThresholdsImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "NumberOfHistogramBins: " << m_NumberOfHistogramBins << std::endl;
  os << indent << "NumberOfThresholds: " << m_NumberOfThresholds << std::endl;
  os << indent << "LabelOffset: " << m_LabelOffset << std::endl;;
  os << indent << "Thresholds: " << std::endl;
  for (unsigned long j=0; j<m_Thresholds.size(); j++)
    {
    os << "\tThreshold #" << j << ": " 
       << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_Thresholds[j]) 
       << std::endl;
    }
}


}// end namespace itk
#endif
