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
#ifndef itkWaveletFrequencyFilterBankGenerator_hxx
#define itkWaveletFrequencyFilterBankGenerator_hxx
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkNumericTraits.h"

namespace itk
{
template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::
  WaveletFrequencyFilterBankGenerator()
  : m_HighPassSubBands(0)
  , m_ScaleFactor(1)
  , m_InverseBank(false)
{
  this->SetHighPassSubBands(1);
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
void
WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::SetInverseBankOn()
{
  this->SetInverseBank(true);
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
void
WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::SetHighPassSubBands(
  unsigned int k)
{
  if (m_HighPassSubBands == k)
    return;

  this->m_HighPassSubBands = k;
  this->SetNumberOfRequiredOutputs(k + 1);
  this->Modified();

  for (unsigned int band = 0; band < this->m_HighPassSubBands + 1; ++band)
    this->SetNthOutput(band, this->MakeOutput(band));
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
void
WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "HighPassSubBands: " << this->m_HighPassSubBands << indent
     << "InverseBank: " << (this->m_InverseBank ? "true" : "false") << std::endl;
}

/* ******* Get Outputs *****/
template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
typename WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::
  OutputImagePointer
  WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::GetOutputLowPass()
{
  return this->GetOutput(0);
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
typename WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::
  OutputImagePointer
  WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::GetOutputHighPass()
{
  return this->GetOutput(this->m_HighPassSubBands);
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
typename WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::
  OutputImagePointer
  WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::GetOutputSubBand(
    unsigned int k)
{
  if (k == 0)
    return this->GetOutputLowPass();
  if (k == m_HighPassSubBands)
    return this->GetOutputHighPass();
  return this->GetOutput(k);
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
std::vector<typename WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::
              OutputImagePointer>
WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::GetOutputsAll()
{
  std::vector<OutputImagePointer> outputList;
  for (unsigned int band = 0; band < this->m_HighPassSubBands + 1; ++band)
  {
    outputList.push_back(this->GetOutputSubBand(band));
  }
  return outputList;
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
std::vector<typename WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::
              OutputImagePointer>
WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::GetOutputsHighPassBands()
{
  std::vector<OutputImagePointer> outputList;
  for (unsigned int band = 1; band < this->m_HighPassSubBands + 1; ++band)
  {
    outputList.push_back(this->GetOutputSubBand(band));
  }
  return outputList;
}

template <typename TOutputImage, typename TWaveletFunction, typename TFrequencyRegionIterator>
void
WaveletFrequencyFilterBankGenerator<TOutputImage, TWaveletFunction, TFrequencyRegionIterator>::GenerateData()
{
  typename TWaveletFunction::Pointer evaluator = TWaveletFunction::New();
  evaluator->SetHighPassSubBands(this->m_HighPassSubBands);

  /***************** Allocate Outputs *****************/
  std::vector<OutputImagePointer>   outputList;
  std::vector<OutputRegionIterator> outputItList;
  for (unsigned int band = 0; band < this->m_HighPassSubBands + 1; ++band)
  {
    outputList.push_back(this->GetOutput(band));
    OutputImagePointer & outputPtr = outputList.back();
    // TODO maybe you need a GenerateOutputInformation instead of setting here metadata.
    // outputPtr->SetOrigin(this->GetOrigin());
    // outputPtr->SetSpacing(this->GetSpacing());
    // outputPtr->SetDirection(this->GetDirection());
    // GenerateImageSource superclass allocates primary output, so use its region.
    outputPtr->SetRegions(outputList[0]->GetLargestPossibleRegion());
    outputPtr->Allocate();
    outputPtr->FillBuffer(0);
    outputItList.push_back(OutputRegionIterator(outputPtr, outputPtr->GetRequestedRegion()));
    outputItList.back().GoToBegin();
  }

  /***************** Set Outputs *****************/
  FunctionValueType w(0);
  // Iterator to calculate frequency modulo only once (optimization)
  OutputRegionIterator frequencyIt(outputList[0], outputList[0]->GetRequestedRegion());
  for (frequencyIt.GoToBegin(); !frequencyIt.IsAtEnd(); ++frequencyIt)
  {
    w = static_cast<FunctionValueType>(sqrt(frequencyIt.GetFrequencyModuloSquare()));

    FunctionValueType evaluatedSubBand;
    // l = 0 is low pass filter, l = m_HighPassSubBands is high-pass filter.
    for (unsigned int l = 0; l < m_HighPassSubBands + 1; ++l)
    {
      evaluatedSubBand = this->m_InverseBank ? evaluator->EvaluateInverseSubBand(w * this->m_ScaleFactor, l)
                                             : evaluator->EvaluateForwardSubBand(w * this->m_ScaleFactor, l);

      outputItList[l].Set(outputItList[l].Get() +
                          static_cast<typename OutputImageType::PixelType::value_type>(evaluatedSubBand));
      ++outputItList[l];
    }
    itkDebugMacro(<< "w_vector: " << frequencyIt.GetFrequencyIndex() << " w: " << w << "  frequencyItIndex: "
                  << frequencyIt.GetIndex() << "  Evaluate highest subband: " << evaluatedSubBand
                  << " outputIndex: " << outputItList[m_HighPassSubBands].GetIndex());
  }
}
} // end namespace itk
#endif
