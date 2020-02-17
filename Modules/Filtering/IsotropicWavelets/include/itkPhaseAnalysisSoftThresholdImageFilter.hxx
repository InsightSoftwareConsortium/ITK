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
#ifndef itkPhaseAnalysisSoftThresholdImageFilter_hxx
#define itkPhaseAnalysisSoftThresholdImageFilter_hxx
#include "itkPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkImageScanlineConstIterator.h"
#include "itkImageScanlineIterator.h"

#include "itkStatisticsImageFilter.h"
namespace itk
{
template <typename TInputImage, typename TOutputImage>
PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::PhaseAnalysisSoftThresholdImageFilter()
  : m_NumOfSigmas(2.0)
  , m_MeanAmp(0)
  , m_SigmaAmp(0)
  , m_Threshold(0)
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(3);
  for (unsigned int n_output = 0; n_output < 3; ++n_output)
  {
    this->SetNthOutput(n_output, this->MakeOutput(n_output));
  }

  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold : " << m_Threshold << std::endl;
  os << indent << "Mean Amplitude : " << m_MeanAmp << std::endl;
  os << indent << "Sigma Amplitude: " << m_SigmaAmp << std::endl;
}

template <typename TInputImage, typename TOutputImage>
void
PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  // Populate and compute outputs from superclass (threaded)
  Superclass::GenerateData();
  auto amplitudePtr = this->GetOutputAmplitude();
  // Compute mean/variance only once.
  if (this->GetApplySoftThreshold())
  {
    using StatisticsImageFilter = itk::StatisticsImageFilter<OutputImageType>;
    auto statsFilter = StatisticsImageFilter::New();
    statsFilter->SetInput(amplitudePtr);
    statsFilter->Update();
    this->m_MeanAmp = statsFilter->GetMean();
    this->m_SigmaAmp = sqrt(statsFilter->GetVariance());
    this->m_Threshold = this->m_MeanAmp + this->m_NumOfSigmas * this->m_SigmaAmp;
  }

  this->GetMultiThreader()->template ParallelizeImageRegion<TOutputImage::ImageDimension>(
    this->GetOutput()->GetRequestedRegion(),
    [this](const OutputImageRegionType & outputRegionForThread) {
      this->ThreadedComputeCosineOfPhase(outputRegionForThread);
    },
    nullptr);
}

template <typename TInputImage, typename TOutputImage>
void
PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ThreadedComputeCosineOfPhase(
  const OutputImageRegionType & outputRegionForThread)
{

  auto phasePtr = this->GetOutputPhase();
  auto amplitudePtr = this->GetOutputAmplitude();
  auto outputPtr = this->GetOutputCosPhase();

  // Set output to cos(phase) applying SoftThreshold if requested.
  OutputImageRegionIterator outIt(outputPtr, outputRegionForThread);
  using OutputImageRegionConstIterator = typename itk::ImageScanlineConstIterator<OutputImageType>;
  OutputImageRegionConstIterator ampIt(amplitudePtr, outputRegionForThread);
  OutputImageRegionConstIterator phaseIt(phasePtr, outputRegionForThread);

  outIt.GoToBegin(), ampIt.GoToBegin(), phaseIt.GoToBegin();
  while (!outIt.IsAtEnd())
  {
    while (!outIt.IsAtEndOfLine())
    {
      OutputImagePixelType out_value = cos(phaseIt.Get());
      if (this->GetApplySoftThreshold())
      {
        if (ampIt.Get() < this->m_Threshold)
        {
          out_value *= ampIt.Get() / this->m_Threshold;
        }
      }
      outIt.Set(out_value);
      ++outIt, ++ampIt, ++phaseIt;
    }

    outIt.NextLine(), ampIt.NextLine(), phaseIt.NextLine();
  }
}
} // end namespace itk
#endif
