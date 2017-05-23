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

#include "itkProgressReporter.h"
#include "itkStatisticsImageFilter.h"
namespace itk
{
template <typename TInputImage, typename TOutputImage>
PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::PhaseAnalysisSoftThresholdImageFilter()
  : m_ApplySoftThreshold(true)
  , m_NumOfSigmas(2.0)
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
PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  unsigned int nC = this->GetInput()->GetNumberOfComponentsPerPixel();

  if (nC < 2)
  {
    itkExceptionMacro(<< "Number of components of input image (" << nC
                      << ") is less than 2. PhaseAnalysis require at least 2 components.");
  }

  // Instead of using GetNumberOfThreads, we need to split the image into the
  // number of regions that will actually be returned by
  // itkImageSource::SplitRequestedRegion. Sometimes this number is less than
  // the number of threads requested.
  OutputImageRegionType dummy;
  unsigned int          actualThreads = this->SplitRequestedRegion(0, this->GetNumberOfThreads(), dummy);

  m_Barrier1 = Barrier::New();
  m_Barrier1->Initialize(actualThreads);
  m_Barrier2 = Barrier::New();
  m_Barrier2->Initialize(actualThreads);
}

template <typename TInputImage, typename TOutputImage>
void
PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  Superclass::ThreadedGenerateData(outputRegionForThread, threadId);

  typename OutputImageType::Pointer phasePtr = this->GetOutputPhase();
  typename OutputImageType::Pointer amplitudePtr = this->GetOutputAmplitude();
  typename OutputImageType::Pointer outputPtr = this->GetOutputCosPhase();

  // Wait for mean/variance calculation. Stats only once.
  if (this->GetApplySoftThreshold())
  {
    m_Barrier1->Wait();
    if (threadId == this->GetNumberOfThreads() - 1)
    {
      typedef itk::StatisticsImageFilter<OutputImageType> StatisticsImageFilter;
      typename StatisticsImageFilter::Pointer             statsFilter = StatisticsImageFilter::New();
      statsFilter->SetInput(amplitudePtr);
      statsFilter->Update();
      this->m_MeanAmp = statsFilter->GetMean();
      this->m_SigmaAmp = sqrt(statsFilter->GetVariance());
      this->m_Threshold = this->m_MeanAmp + this->m_NumOfSigmas * this->m_SigmaAmp;
    }
    m_Barrier2->Wait();
  }

  // Set output to cos(phase) applying SoftThreshold if requested.
  typedef typename itk::ImageScanlineIterator<OutputImageType>      OutputImageRegionIterator;
  OutputImageRegionIterator                                         outIt(outputPtr, outputRegionForThread);
  typedef typename itk::ImageScanlineConstIterator<OutputImageType> OutputImageRegionConstIterator;
  OutputImageRegionConstIterator                                    ampIt(amplitudePtr, outputRegionForThread);
  OutputImageRegionConstIterator                                    phaseIt(phasePtr, outputRegionForThread);

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
    progress.CompletedPixel(); // Per line
  }
}
} // end namespace itk
#endif
