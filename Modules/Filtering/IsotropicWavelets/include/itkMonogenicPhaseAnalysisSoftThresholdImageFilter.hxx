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
#ifndef itkMonogenicPhaseAnalysisSoftThresholdImageFilter_hxx
#define itkMonogenicPhaseAnalysisSoftThresholdImageFilter_hxx
#include "itkMonogenicPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageIterator.h"

#include "itkProgressReporter.h"
#include "itkStatisticsImageFilter.h"
#include "itkRieszFrequencyFunction.h"
namespace itk
{
template <typename TInputImage, typename TOutputImage>
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage,
                                               TOutputImage>::MonogenicPhaseAnalysisSoftThresholdImageFilter()
  : m_ApplySoftThreshold(true)
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(3);

  for (unsigned int n_output = 0; n_output < 3; ++n_output)
  {
    this->SetNthOutput(n_output, this->MakeOutput(n_output));
  }
}

template <typename TInputImage, typename TOutputImage>
void
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                     Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold : " << m_Threshold << std::endl;
  os << indent << "Mean Amplitude : " << m_MeanAmp << std::endl;
  os << indent << "Sigma Amplitude: " << m_SigmaAmp << std::endl;
}

template <typename TInputImage, typename TOutputImage>
void
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  m_NC = this->GetInput()->GetNumberOfComponentsPerPixel();
  if (this->GetInput()->GetNumberOfComponentsPerPixel() != ImageDimension + 1)
  {
    itkExceptionMacro(<< "Number of components of input image (" << m_NC << ") is not ImageDimension+1 ("
                      << ImageDimension + 1 << ")");
  }

  ThreadIdType nbOfThreads = this->GetNumberOfThreads();
  m_Barrier1 = Barrier::New();
  m_Barrier1->Initialize(nbOfThreads);
  m_Barrier2 = Barrier::New();
  m_Barrier2->Initialize(nbOfThreads);

  m_MeanAmp = 0;
  m_SigmaAmp = 0;
  m_Threshold = 0;
}

template <typename TInputImage, typename TOutputImage>
void
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  typename OutputImageType::Pointer outputPtr = this->GetOutput();
  typename OutputImageType::Pointer amplitudePtr = this->GetOutput(1);
  typename OutputImageType::Pointer phasePtr = this->GetOutput(2);

  OutputImageRegionIterator outIt(outputPtr, outputRegionForThread);
  OutputImageRegionIterator ampIt(amplitudePtr, outputRegionForThread);
  OutputImageRegionIterator phaseIt(phasePtr, outputRegionForThread);

  InputImageRegionConstIterator monoIt(this->GetInput(), outputRegionForThread);
  for (monoIt.GoToBegin(), ampIt.GoToBegin(), phaseIt.GoToBegin(); !monoIt.IsAtEnd(); ++monoIt, ++ampIt, ++phaseIt)
  {
    InputImagePixelType  vecValue = monoIt.Get();
    OutputImagePixelType rieszNormSquare = this->ComputeRieszNormSquare(vecValue);
    ampIt.Set(this->ComputeAmplitude(vecValue, rieszNormSquare));
    phaseIt.Set(cos(this->ComputePhase(vecValue, rieszNormSquare)));
  }

  // Wait for mean/variance calculation. Stats only once.
  m_Barrier1->Wait();
  if (threadId == this->GetNumberOfThreads() - 1)
  {
    typedef itk::StatisticsImageFilter<OutputImageType> StatisticsImageFilter;
    typename StatisticsImageFilter::Pointer             statsFilter = StatisticsImageFilter::New();
    statsFilter->SetInput(amplitudePtr);
    statsFilter->Update();
    m_MeanAmp = statsFilter->GetMean();
    m_SigmaAmp = sqrt(statsFilter->GetVariance());
    m_Threshold = m_MeanAmp + 2.0 * m_SigmaAmp;
  }
  m_Barrier2->Wait();

  for (outIt.GoToBegin(), ampIt.GoToBegin(), phaseIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt, ++ampIt, ++phaseIt)
  {
    OutputImagePixelType out_value = cos(phaseIt.Get());

    if (this->GetApplySoftThreshold() == true)
      if (ampIt.Get() < m_Threshold)
        out_value = out_value * ampIt.Get() / m_Threshold;

    outIt.Set(out_value);
    progress.CompletedPixel();
  }
}

template <typename TInputImage, typename TOutputImage>
typename MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ComputeRieszNormSquare(
  const InputImagePixelType & monoPixel) const
{

  OutputImagePixelType out(0);
  for (unsigned int r = 1; r < this->GetNC(); r++)
  {
    out += monoPixel[r] * monoPixel[r];
  }
  return out;
}

template <typename TInputImage, typename TOutputImage>
typename MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ComputeAmplitude(
  const InputImagePixelType &  monoPixel,
  const OutputImagePixelType & rieszNormSquare) const
{
  return sqrt(monoPixel[0] * monoPixel[0] + rieszNormSquare);
}

template <typename TInputImage, typename TOutputImage>
typename MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ComputePhase(
  const InputImagePixelType &  monoPixel,
  const OutputImagePixelType & rieszNormSquare) const
{
  return atan2(sqrt(rieszNormSquare), monoPixel[0]);
}

template <typename TInputImage, typename TOutputImage>
FixedArray<typename MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::OutputImagePixelType,
           MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ImageDimension - 1>
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ComputePhaseOrientation(
  const InputImagePixelType &  monoPixel,
  const OutputImagePixelType & rieszNormSquare) const
{
  // the angles of the polar coordinates of the normed vector:
  // V = (R1*f, ..., Rn*f) / RieszNorm
  FixedArray<OutputImagePixelType, ImageDimension - 1> out;
  out.Fill(NumericTraits<OutputImagePixelType>::ZeroValue());
  OutputImagePixelType rNorm = sqrt(rieszNormSquare);
  OutputImagePixelType r1Unitary = monoPixel[1] / rNorm;
  for (unsigned int i = 0; i < ImageDimension - 1; i++)
  {
    out[i] = atan2(monoPixel[i + 2] / rNorm, r1Unitary) + ((monoPixel[i + 2] >= 0) ? 0 : itk::Math::pi);
  }
  return out;
}

template <typename TInputImage, typename TOutputImage>
typename MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
MonogenicPhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ComputeRieszProjection(
  const InputImagePixelType & monoPixel,
  const DirectionType &       direction) const
{
  OutputImagePixelType out(0);
  for (unsigned int r = 1; r < this->GetNC(); r++)
    out += direction[r] * monoPixel[r];
  return out;
}
} // end namespace itk
#endif
