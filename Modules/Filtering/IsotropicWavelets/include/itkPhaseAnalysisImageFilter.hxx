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

  this->DynamicMultiThreadingOn();
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
PhaseAnalysisImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
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
      phaseIt.Set(this->ComputePhase(vecValue, featureAmpSquare));
      ++inputIt;
      ++ampIt;
      ++phaseIt;
    }

    inputIt.NextLine();
    ampIt.NextLine();
    phaseIt.NextLine();
  }
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
