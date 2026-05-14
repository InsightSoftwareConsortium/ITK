/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkWaveletFrequencyForwardUndecimated_hxx
#define itkWaveletFrequencyForwardUndecimated_hxx
#include <itkCastImageFilter.h>
#include <itkImage.h>
#include <algorithm>
#include <itkMultiplyImageFilter.h>
#include <itkChangeInformationImageFilter.h>
#include <itkWaveletUtilities.h>

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::WaveletFrequencyForwardUndecimated()


{
  this->SetNumberOfRequiredInputs(1);
  m_WaveletFilterBank = WaveletFilterBankType::New();
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
std::pair<unsigned int, unsigned int>
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::OutputIndexToLevelBand(
  unsigned int linear_index)
{
  return itk::utils::IndexToLevelBandSteerablePyramid(linear_index, this->m_Levels, this->m_HighPassSubBands);
};

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
typename WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::OutputsType
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputs()
{
  OutputsType outputPtrs;
  for (unsigned int nout = 0; nout < this->m_TotalOutputs; ++nout)
  {
    outputPtrs.push_back(this->GetOutput(nout));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
typename WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::OutputsType
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputsHighPass()
{
  OutputsType outputPtrs;
  for (unsigned int nout = 1; nout < this->m_TotalOutputs; ++nout)
  {
    outputPtrs.push_back(this->GetOutput(nout));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
typename WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::OutputImagePointer
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputLowPass()
{
  return this->GetOutput(this->m_TotalOutputs - 1);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
typename WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::OutputsType
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputsHighPassByLevel(
  unsigned int level)
{
  OutputsType  outputPtrs;
  unsigned int nOutput_start = level * this->m_HighPassSubBands;
  unsigned int nOutput_end = (level + 1) * this->m_HighPassSubBands;
  if (nOutput_end > this->m_TotalOutputs)
  {
    nOutput_end = this->m_TotalOutputs;
  }
  for (unsigned int nOutput = nOutput_start; nOutput < nOutput_end; ++nOutput)
  {
    outputPtrs.push_back(this->GetOutput(nOutput));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
unsigned int
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::ComputeMaxNumberOfLevels(
  const typename InputImageType::SizeType & inputSize,
  const unsigned int                        scaleFactor)
{
  return itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::SetLevels(unsigned int inputLevels)
{
  unsigned int current_outputs = 1 + this->m_Levels * this->m_HighPassSubBands;

  if (this->m_TotalOutputs == current_outputs && this->m_Levels == inputLevels)
  {
    return;
  }

  this->m_Levels = inputLevels;
  this->m_TotalOutputs = 1 + inputLevels * this->m_HighPassSubBands;

  this->SetNumberOfRequiredOutputs(this->m_TotalOutputs);
  this->Modified();
  for (unsigned int n_output = 0; n_output < this->m_TotalOutputs; ++n_output)
  {
    this->SetNthOutput(n_output, this->MakeOutput(n_output));
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::SetHighPassSubBands(unsigned int k)
{
  if (this->m_HighPassSubBands == k)
  {
    return;
  }
  this->m_HighPassSubBands = k;
  // Trigger setting new number of outputs avoiding code duplication
  this->SetLevels(this->m_Levels);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::PrintSelf(std::ostream & os,
                                                                                             Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << " Levels: " << this->m_Levels << " HighPassSubBands: " << this->m_HighPassSubBands
     << " TotalOutputs: " << this->m_TotalOutputs << std::endl;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer inputPtr = this->GetInput();

  if (!inputPtr)
  {
    itkExceptionMacro(<< "Input has not been set");
  }

  /** inputOrigin and inputSpacing is lost and should be restored
   * at the end of the inverse wavelet transform. */
  typename OutputImageType::PointType   inputModifiedOrigin(0);
  typename OutputImageType::SpacingType inputModifiedSpacing(1);
  // typename OutputImageType::DirectionType outputDirection = inputDirection;

  OutputImagePointer outputPtr;
  for (unsigned int nOutput = 0; nOutput < this->m_TotalOutputs; ++nOutput)
  {
    outputPtr = this->GetOutput(nOutput);
    if (!outputPtr)
    {
      continue;
    }
    outputPtr->SetLargestPossibleRegion(inputPtr->GetLargestPossibleRegion());
    outputPtr->SetOrigin(inputModifiedOrigin);
    outputPtr->SetSpacing(inputModifiedSpacing);
    outputPtr->SetDirection(inputPtr->GetDirection());
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateOutputRequestedRegion(
  DataObject * refOutput)
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion(refOutput);

  // find the index for this output
  auto refIndex = static_cast<unsigned int>(refOutput->GetSourceOutputIndex());

  auto * ptr = itkDynamicCastInDebugMode<TOutputImage *>(refOutput);
  if (!ptr)
  {
    itkExceptionMacro(<< "Could not cast refOutput to TOutputImage*.");
  }

  for (unsigned int nOutput = 0; nOutput < this->m_TotalOutputs; ++nOutput)
  {
    if (nOutput == refIndex)
    {
      continue;
    }
    if (!this->GetOutput(nOutput))
    {
      continue;
    }
    this->GetOutput(nOutput)->SetRequestedRegion(ptr->GetRequestedRegion());
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr = const_cast<InputImageType *>(this->GetInput());
  if (!inputPtr)
  {
    itkExceptionMacro(<< "Input has not been set.");
  }

  // compute baseIndex and baseSize
  using SizeType = typename OutputImageType::SizeType;
  using IndexType = typename OutputImageType::IndexType;
  using RegionType = typename OutputImageType::RegionType;

  // All the levels and subbands have the same size.
  // At least one band is ensured to exist, so use it.
  unsigned int refOutput = 0;
  SizeType     baseSize = this->GetOutput(refOutput)->GetRequestedRegion().GetSize();
  IndexType    baseIndex = this->GetOutput(refOutput)->GetRequestedRegion().GetIndex();
  RegionType   baseRegion;

  baseRegion.SetIndex(baseIndex);
  baseRegion.SetSize(baseSize);

  // set the input requested region
  inputPtr->SetRequestedRegion(baseRegion);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForwardUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateData()
{
  InputImageConstPointer input = this->GetInput();

  this->AllocateOutputs();

  // note: clear reduces size to zero, but doesn't change capacity.
  m_WaveletFilterBankPyramid.clear();

  using CastFilterType = itk::CastImageFilter<InputImageType, OutputImageType>;
  auto castFilter = CastFilterType::New();
  castFilter->SetInput(input);
  castFilter->Update();
  OutputImagePointer inputPerLevel = castFilter->GetOutput();
  using ChangeInformationFilterType = itk::ChangeInformationImageFilter<OutputImageType>;
  auto                                   changeInputInfoFilter = ChangeInformationFilterType::New();
  typename InputImageType::PointType     origin_old = inputPerLevel->GetOrigin();
  typename InputImageType::SpacingType   spacing_old = inputPerLevel->GetSpacing();
  typename InputImageType::DirectionType direction_old = inputPerLevel->GetDirection();
  typename InputImageType::PointType     origin_new = origin_old;
  origin_new.Fill(0);
  typename InputImageType::SpacingType spacing_new = spacing_old;
  spacing_new.Fill(1);
  typename InputImageType::DirectionType direction_new = direction_old;
  direction_new.SetIdentity();
  changeInputInfoFilter->SetInput(inputPerLevel);
  changeInputInfoFilter->ChangeRegionOff();
  changeInputInfoFilter->ChangeDirectionOn();
  changeInputInfoFilter->ChangeSpacingOn();
  changeInputInfoFilter->ChangeOriginOn();
  changeInputInfoFilter->UseReferenceImageOff();
  changeInputInfoFilter->SetOutputOrigin(origin_new);
  changeInputInfoFilter->SetOutputSpacing(spacing_new);
  changeInputInfoFilter->SetOutputDirection(direction_new);
  changeInputInfoFilter->Update();

  // Generate WaveletFilterBank.
  this->m_WaveletFilterBank->SetHighPassSubBands(this->m_HighPassSubBands);
  this->m_WaveletFilterBank->SetSize(changeInputInfoFilter->GetOutput()->GetLargestPossibleRegion().GetSize());
  this->m_WaveletFilterBank->Update();
  OutputsType        highPassWavelets = this->m_WaveletFilterBank->GetOutputsHighPassBands();
  OutputImagePointer lowPassWavelet = this->m_WaveletFilterBank->GetOutputLowPass();

  if (this->m_StoreWaveletFilterBankPyramid)
  {
    for (unsigned int bankOutput = 0; bankOutput < this->m_HighPassSubBands + 1; ++bankOutput)
    {
      this->m_WaveletFilterBankPyramid.push_back(this->m_WaveletFilterBank->GetOutput(bankOutput));
    }
  }

  using MultiplyFilterType = itk::MultiplyImageFilter<OutputImageType>;
  inputPerLevel = changeInputInfoFilter->GetOutput();
  auto scaleFactor = static_cast<double>(this->m_ScaleFactor);
  for (unsigned int level = 0; level < this->m_Levels; ++level)
  {
    /******* Set HighPass bands *****/
    itkDebugMacro(<< "Number of FilterBank high pass bands: " << highPassWavelets.size());
    for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
    {
      unsigned int n_output = level * this->m_HighPassSubBands + band;
      /******* Band dilation factor for HighPass bands *****/
      //  2^(1/#bands) instead of Dyadic dilations.
      auto multiplyByAnalysisBandFactor = MultiplyFilterType::New();
      multiplyByAnalysisBandFactor->SetInput1(highPassWavelets[band]);
      // double expBandFactor = 0;
      // double expBandFactor = - static_cast<double>(level*ImageDimension)/2.0;
      double expBandFactor =
        (-static_cast<double>(level + 1) + band / static_cast<double>(this->m_HighPassSubBands)) * ImageDimension / 2.0;
      multiplyByAnalysisBandFactor->SetConstant(std::pow(scaleFactor, expBandFactor));
      // TODO Warning: InPlace here deletes buffered region of input.
      // https://public.kitware.com/pipermail/community/2015-April/008819.html
      // multiplyByAnalysisBandFactor->InPlaceOn();
      multiplyByAnalysisBandFactor->Update();

      auto multiplyHighBandFilter = MultiplyFilterType::New();
      multiplyHighBandFilter->SetInput1(multiplyByAnalysisBandFactor->GetOutput());
      multiplyHighBandFilter->SetInput2(inputPerLevel);
      multiplyHighBandFilter->InPlaceOn();
      multiplyHighBandFilter->GraftOutput(this->GetOutput(n_output));
      multiplyHighBandFilter->Update();

      this->UpdateProgress(static_cast<float>(n_output - 1) / static_cast<float>(m_TotalOutputs));
      this->GraftNthOutput(n_output, multiplyHighBandFilter->GetOutput());
    }

    /******* Calculate LowPass band *****/
    auto multiplyLowFilter = MultiplyFilterType::New();
    multiplyLowFilter->SetInput1(lowPassWavelet);
    multiplyLowFilter->SetInput2(inputPerLevel);
    // multiplyLowFilter->InPlaceOn();
    multiplyLowFilter->Update();
    if (level == this->m_Levels - 1) // Set low_pass output (index=this->m_TotalOutputs - 1)
    {
      auto multiplyByLevelFactor = MultiplyFilterType::New();
      multiplyByLevelFactor->SetInput1(multiplyLowFilter->GetOutput());
      double expLevelFactor = (-static_cast<double>(this->m_Levels * ImageDimension)) / 2.0;
      multiplyByLevelFactor->SetConstant(std::pow(scaleFactor, expLevelFactor));
      multiplyByLevelFactor->Update();
      this->GraftNthOutput(this->m_TotalOutputs - 1, multiplyByLevelFactor->GetOutput());
      this->UpdateProgress(static_cast<float>(this->m_TotalOutputs - 1) / static_cast<float>(this->m_TotalOutputs));
      continue;
    }
    else // update inputPerLevel
    {
      inputPerLevel = multiplyLowFilter->GetOutput();
      /******* Downsample (scale) wavelets *****/
      m_WaveletFilterBank->SetLevel(level + 1);
      m_WaveletFilterBank->Update();
      lowPassWavelet = m_WaveletFilterBank->GetOutputLowPass();
      lowPassWavelet->DisconnectPipeline();
      highPassWavelets = m_WaveletFilterBank->GetOutputsHighPassBands();
      for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
      {
        highPassWavelets[band]->DisconnectPipeline();
      }

      if (this->m_StoreWaveletFilterBankPyramid)
      {
        m_WaveletFilterBankPyramid.push_back(lowPassWavelet);
        for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
        {
          m_WaveletFilterBankPyramid.push_back(highPassWavelets[band]);
        }
      }
    } // end update inputPerLevel
  } // end level
}
} // end namespace itk
#endif
