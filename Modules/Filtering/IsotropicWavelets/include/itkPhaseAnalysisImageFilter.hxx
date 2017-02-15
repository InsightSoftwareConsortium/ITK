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
#ifndef itkPhaseAnalysisImageFilter_hxx
#define itkPhaseAnalysisImageFilter_hxx
#include "itkPhaseAnalysisImageFilter.h"
#include "itkProgressReporter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::PhaseAnalysisImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(2);

  for (unsigned int n_output = 0; n_output < 2; ++n_output)
  {
    this->SetNthOutput(n_output, this->MakeOutput(n_output));
  }
}

template <typename TInputImage, typename TOutputImage>
void
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TInputImage, typename TOutputImage>
void
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  unsigned int nC = this->GetInput()->GetNumberOfComponentsPerPixel();

  if (nC < 2)
  {
    itkExceptionMacro(<< "Number of components of input image (" << nC
                      << ") is less than 2. PhaseAnalysis require at least 2 components.");
  }
}

template <typename TInputImage, typename TOutputImage>
void
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{
  ProgressReporter progress(
    this, threadId, outputRegionForThread.GetNumberOfPixels() / outputRegionForThread.GetSize()[0]);

  typename OutputImageType::Pointer phasePtr = this->GetOutputPhase();
  typename OutputImageType::Pointer amplitudePtr = this->GetOutputAmplitude();

  OutputImageRegionIterator     ampIt(amplitudePtr, outputRegionForThread);
  OutputImageRegionIterator     phaseIt(phasePtr, outputRegionForThread);
  InputImageRegionConstIterator inputIt(this->GetInput(), outputRegionForThread);

  InputImagePixelType  vecValue;
  OutputImagePixelType featureAmpSquare;
  inputIt.GoToBegin();
  ampIt.GoToBegin();
  phaseIt.GoToBegin();
  while (!inputIt.IsAtEnd())
  {
    while (!inputIt.IsAtEndOfLine())
    {
      vecValue = inputIt.Get();
      featureAmpSquare = this->ComputeFeatureVectorNormSquare(vecValue);
      ampIt.Set(this->ComputeAmplitude(vecValue, featureAmpSquare));
      phaseIt.Set(cos(this->ComputePhase(vecValue, featureAmpSquare)));
      ++inputIt;
      ++ampIt;
      ++phaseIt;
    }
    inputIt.NextLine();
    ampIt.NextLine();
    phaseIt.NextLine();
    progress.CompletedPixel();
  }
}

template <typename TInputImage, typename TOutputImage>
typename PhaseAnalysisImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::ComputeFeatureVectorNormSquare(
  const InputImagePixelType & inputPixel) const
{
  const unsigned int & nC = this->GetInput()->GetNumberOfComponentsPerPixel();
  OutputImagePixelType out(0);

  for (unsigned int r = 1; r < nC; r++)
  {
    out += inputPixel[r] * inputPixel[r];
  }
  return out;
}

template <typename TInputImage, typename TOutputImage>
typename PhaseAnalysisImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::ComputeAmplitude(
  const InputImagePixelType &  inputPixel,
  const OutputImagePixelType & featureAmpSquare) const
{
  return sqrt(inputPixel[0] * inputPixel[0] + featureAmpSquare);
}

template <typename TInputImage, typename TOutputImage>
typename PhaseAnalysisImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::ComputePhase(const InputImagePixelType &  inputPixel,
                                                                  const OutputImagePixelType & featureAmpSquare) const
{
  return atan2(sqrt(featureAmpSquare), inputPixel[0]);
}

template <typename TInputImage, typename TOutputImage>
itk::FixedArray<typename PhaseAnalysisImageFilter<TInputImage, TOutputImage>::OutputImagePixelType,
                PhaseAnalysisImageFilter<TInputImage, TOutputImage>::ImageDimension - 1>
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::ComputePhaseOrientation(
  const InputImagePixelType &  inputPixel,
  const OutputImagePixelType & featureAmpSquare) const
{
  // the angles of the polar coordinates of the normed vector:
  // V = (R1*f, ..., Rn*f) / FeatureNorm
  FixedArray<OutputImagePixelType, ImageDimension - 1> out;
  out.Fill(NumericTraits<OutputImagePixelType>::ZeroValue());
  OutputImagePixelType fNorm = sqrt(featureAmpSquare);
  OutputImagePixelType f1Unitary = inputPixel[1] / fNorm;
  for (unsigned int i = 0; i < ImageDimension - 1; i++)
  {
    out[i] = atan2(inputPixel[i + 2] / fNorm, f1Unitary) + ((inputPixel[i + 2] >= 0) ? 0 : itk::Math::pi);
  }
  return out;
}

// template< typename TInputImage, typename TOutputImage >
// typename PhaseAnalysisSoftThresholdImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
// PhaseAnalysisSoftThresholdImageFilter< TInputImage, TOutputImage >
// ::ComputeRieszProjection(
//     const InputImagePixelType & monoPixel,
//     const DirectionType & direction ) const
// {
//   OutputImagePixelType out(0);
//   for (unsigned int r = 1; r < this->GetNC(); r++)
//     out += direction[r] * monoPixel[r];
//   return out;
// }
} // end namespace itk
#endif
