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
#ifndef itkWaveletFrequencyForward_hxx
#define itkWaveletFrequencyForward_hxx
#include <itkCastImageFilter.h>
#include <itkImage.h>
#include <algorithm>
#include <itkMultiplyImageFilter.h>
#include <itkShrinkDecimateImageFilter.h>
#include <itkChangeInformationImageFilter.h>
#include <itkWaveletUtilities.h>

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  WaveletFrequencyForward()


{
  this->SetNumberOfRequiredInputs(1);
  m_WaveletFilterBank = WaveletFilterBankType::New();
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
std::pair<unsigned int, unsigned int>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  OutputIndexToLevelBand(unsigned int linear_index)
{
  return itk::utils::IndexToLevelBandSteerablePyramid(linear_index, this->m_Levels, this->m_HighPassSubBands);
};

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::OutputsType
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::GetOutputs()
{
  OutputsType outputPtrs;
  for (unsigned int nout = 0; nout < this->m_TotalOutputs; ++nout)
  {
    outputPtrs.push_back(this->GetOutput(nout));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::OutputsType
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::GetOutputsHighPass()
{
  OutputsType outputPtrs;
  for (unsigned int nout = 1; nout < this->m_TotalOutputs; ++nout)
  {
    outputPtrs.push_back(this->GetOutput(nout));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  OutputImagePointer
  WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::GetOutputLowPass()
{
  return this->GetOutput(this->m_TotalOutputs - 1);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::OutputsType
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  GetOutputsHighPassByLevel(unsigned int level)
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

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
unsigned int
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  ComputeMaxNumberOfLevels(const typename InputImageType::SizeType & inputSize, const unsigned int scaleFactor)
{
  return itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::SetLevels(
  unsigned int inputLevels)
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

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::SetHighPassSubBands(
  unsigned int k)
{
  if (this->m_HighPassSubBands == k)
  {
    return;
  }
  this->m_HighPassSubBands = k;
  // Trigger setting new number of outputs avoiding code duplication
  this->SetLevels(this->m_Levels);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << " Levels: " << this->m_Levels << " HighPassSubBands: " << this->m_HighPassSubBands
     << " TotalOutputs: " << this->m_TotalOutputs << std::endl;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer inputPtr = this->GetInput();

  if (!inputPtr)
  {
    itkExceptionMacro(<< "Input has not been set");
  }

  typename InputImageType::SizeType  inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  typename InputImageType::IndexType inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  /** inputOrigin and inputSpacing is lost and should be restored
   * at the end of the inverse wavelet transform. */
  typename OutputImageType::PointType   inputModifiedOrigin(0);
  typename OutputImageType::SpacingType inputModifiedSpacing(1);
  // typename OutputImageType::DirectionType outputDirection = inputDirection;

  OutputImagePointer                    outputPtr;
  typename OutputImageType::SizeType    inputSizePerLevel = inputSize;
  typename OutputImageType::IndexType   inputStartIndexPerLevel = inputStartIndex;
  typename OutputImageType::PointType   inputOriginPerLevel = inputModifiedOrigin;
  typename OutputImageType::SpacingType inputSpacingPerLevel = inputModifiedSpacing;
  // typename OutputImageType::DirectionType inputDirectionPerLevel = inputDirection;
  // we need to compute the output spacing, the output image size,
  // and the output image start index
  for (unsigned int level = 0; level < this->m_Levels; ++level)
  {
    // Bands per level . No downsampling in the first level iteration.
    for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
    {
      unsigned int current_output = level * m_HighPassSubBands + band;
      outputPtr = this->GetOutput(current_output);
      if (!outputPtr)
      {
        continue;
      }
      typename OutputImageType::RegionType largestPossibleRegion;
      largestPossibleRegion.SetSize(inputSizePerLevel);
      largestPossibleRegion.SetIndex(inputStartIndexPerLevel);
      outputPtr->SetLargestPossibleRegion(largestPossibleRegion);
      outputPtr->SetOrigin(inputOriginPerLevel);
      outputPtr->SetSpacing(inputSpacingPerLevel);
      // outputPtr->SetDirection(outputDirection);
    }
    // Calculate for next levels new Size and Index, per dim.
    for (unsigned int idim = 0; idim < OutputImageType::ImageDimension; idim++)
    {
      // Size divided by scale
      inputSizePerLevel[idim] =
        static_cast<SizeValueType>(std::floor(static_cast<double>(inputSizePerLevel[idim]) / this->m_ScaleFactor));
      if (inputSizePerLevel[idim] < 1)
      {
        inputSizePerLevel[idim] = 1;
      }
      // Index divided by scale
      inputStartIndexPerLevel[idim] = static_cast<IndexValueType>(
        std::ceil(static_cast<double>(inputStartIndexPerLevel[idim]) / this->m_ScaleFactor));
      // Spacing
      inputSpacingPerLevel[idim] = inputSpacingPerLevel[idim] * this->m_ScaleFactor;
      // Origin, the same.
      // inputOriginPerLevel[idim] = inputOriginPerLevel[idim];
      // inputOriginPerLevel[idim] = inputOriginPerLevel[idim] / this->m_ScaleFactor;
    }

    // Set the low pass at the end.
    if (level == this->m_Levels - 1)
    {
      outputPtr = this->GetOutput(this->m_TotalOutputs - 1);
      if (!outputPtr)
      {
        continue;
      }
      typename OutputImageType::RegionType largestPossibleRegion;
      largestPossibleRegion.SetSize(inputSizePerLevel);
      largestPossibleRegion.SetIndex(inputStartIndexPerLevel);
      outputPtr->SetLargestPossibleRegion(largestPossibleRegion);
      outputPtr->SetOrigin(inputOriginPerLevel);
      outputPtr->SetSpacing(inputSpacingPerLevel);
      // outputPtr->SetDirection(inputDirectionPerLevel);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  GenerateOutputRequestedRegion(DataObject * refOutput)
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion(refOutput);

  // find the index for this output
  auto                                  refIndex = static_cast<unsigned int>(refOutput->GetSourceOutputIndex());
  std::pair<unsigned int, unsigned int> pairRef = this->OutputIndexToLevelBand(refIndex);
  unsigned int                          refLevel = pairRef.first;
  // unsigned int refBand  = pairRef.second;

  // compute baseIndex and baseSize
  using SizeType = typename OutputImageType::SizeType;
  using IndexType = typename OutputImageType::IndexType;
  using RegionType = typename OutputImageType::RegionType;

  auto * ptr = itkDynamicCastInDebugMode<TOutputImage *>(refOutput);
  if (!ptr)
  {
    itkExceptionMacro(<< "Could not cast refOutput to TOutputImage*.");
  }

  if (ptr->GetRequestedRegion() == ptr->GetLargestPossibleRegion())
  {
    // set the requested regions for the other outputs to their largest
    for (unsigned int nout = 0; nout < this->m_TotalOutputs; ++nout)
    {
      if (nout == refIndex)
      {
        continue;
      }
      if (!this->GetOutput(nout))
      {
        continue;
      }
      this->GetOutput(nout)->SetRequestedRegionToLargestPossibleRegion();
    }
  }
  else
  {
    // compute requested regions for the other outputs based on
    // the requested region of the reference output
    IndexType  outputIndex;
    SizeType   outputSize;
    RegionType outputRegion;
    RegionType baseRegion = ptr->GetRequestedRegion();
    IndexType  baseIndex = baseRegion.GetIndex();
    SizeType   baseSize = baseRegion.GetSize();
    for (unsigned int level = 0; level < this->m_Levels + 1; ++level)
    {
      int distanceToReferenceLevel = static_cast<int>(refLevel) - static_cast<int>(level);
      for (unsigned int idim = 0; idim < TOutputImage::ImageDimension; idim++)
      {
        outputIndex[idim] =
          baseIndex[idim] *
          static_cast<IndexValueType>(std::pow(static_cast<double>(this->m_ScaleFactor), distanceToReferenceLevel));
        outputSize[idim] =
          baseSize[idim] *
          static_cast<SizeValueType>(std::pow(static_cast<double>(this->m_ScaleFactor), distanceToReferenceLevel));
        if (outputSize[idim] < 1)
        {
          itkExceptionMacro(
            << "Failure at level: " << level
            << " in forward wavelet, going to negative image size. Too many levels for input image size.");
        }
      }
      outputRegion.SetIndex(outputIndex);
      outputRegion.SetSize(outputSize);

      // Set low pass output
      if (level == this->m_Levels)
      {
        unsigned int n_output = this->m_TotalOutputs - 1;
        if (n_output == refIndex)
        {
          continue;
        }
        if (!this->GetOutput(n_output))
        {
          continue;
        }
        outputRegion.Crop(this->GetOutput(n_output)->GetLargestPossibleRegion());
        this->GetOutput(n_output)->SetRequestedRegion(outputRegion);
      }
      else // Bands per level
      {
        for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
        {
          unsigned int n_output = level * this->m_HighPassSubBands + band;
          if (n_output == refIndex)
          {
            continue;
          }
          if (!this->GetOutput(n_output))
          {
            continue;
          }
          outputRegion.Crop(this->GetOutput(n_output)->GetLargestPossibleRegion());
          // set the requested region
          this->GetOutput(n_output)->SetRequestedRegion(outputRegion);
        }
      }
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::
  GenerateInputRequestedRegion()
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

  // The first level has the same size than input.
  // At least one band is ensured to exist, so use it.
  unsigned int refOutput = 0;
  SizeType     baseSize = this->GetOutput(refOutput)->GetRequestedRegion().GetSize();
  IndexType    baseIndex = this->GetOutput(refOutput)->GetRequestedRegion().GetIndex();
  RegionType   baseRegion;

  baseRegion.SetIndex(baseIndex);
  baseRegion.SetSize(baseSize);

  // make sure the requested region is within the largest possible
  baseRegion.Crop(inputPtr->GetLargestPossibleRegion());

  // set the input requested region
  inputPtr->SetRequestedRegion(baseRegion);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank, typename TFrequencyShrinkFilterType>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank, TFrequencyShrinkFilterType>::GenerateData()
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

  // TODO think about passing the FrequencyShrinker as template parameter to work with different FFT layout, or
  // regular images directly in frequency domain.
  // using LocalFrequencyShrinkFilterType = itk::FrequencyShrinkViaInverseFFTImageFilter<OutputImageType>;
  using LocalFrequencyShrinkFilterType = itk::FrequencyShrinkImageFilter<OutputImageType>;
  using ShrinkDecimateFilterType = itk::ShrinkDecimateImageFilter<OutputImageType, OutputImageType>;
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
        (-static_cast<double>(level) + band / static_cast<double>(this->m_HighPassSubBands)) * ImageDimension / 2.0;
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
    inputPerLevel = multiplyLowFilter->GetOutput();

    // Shrink in the frequency domain the stored low band for the next level.
    auto freqShrinkFilter = LocalFrequencyShrinkFilterType::New();
    freqShrinkFilter->SetInput(inputPerLevel);
    freqShrinkFilter->SetShrinkFactors(this->m_ScaleFactor);

    if (level == this->m_Levels - 1) // Set low_pass output (index=this->m_TotalOutputs - 1)
    {
      freqShrinkFilter->GraftOutput(this->GetOutput(this->m_TotalOutputs - 1));
      freqShrinkFilter->Update();
      this->GraftNthOutput(this->m_TotalOutputs - 1, freqShrinkFilter->GetOutput());
      this->UpdateProgress(static_cast<float>(this->m_TotalOutputs - 1) / static_cast<float>(this->m_TotalOutputs));
      continue;
    }
    else // update inputPerLevel
    {
      freqShrinkFilter->Update();
      inputPerLevel = freqShrinkFilter->GetOutput();
      /******* DownSample wavelets *****/
      auto decimateWaveletFilter = ShrinkDecimateFilterType::New();
      decimateWaveletFilter->SetInput(lowPassWavelet);
      decimateWaveletFilter->SetShrinkFactors(this->m_ScaleFactor);
      decimateWaveletFilter->Update();
      auto changeDecimateInfoFilter = ChangeInformationFilterType::New();
      changeDecimateInfoFilter->SetInput(decimateWaveletFilter->GetOutput());
      changeDecimateInfoFilter->ChangeAll();
      changeDecimateInfoFilter->UseReferenceImageOn();
      changeDecimateInfoFilter->SetReferenceImage(inputPerLevel);
      changeDecimateInfoFilter->Update();
      lowPassWavelet = changeDecimateInfoFilter->GetOutput();
      lowPassWavelet->DisconnectPipeline();
      for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
      {
        auto decimateHPWaveletFilter = ShrinkDecimateFilterType::New();
        decimateHPWaveletFilter->SetShrinkFactors(this->m_ScaleFactor);
        decimateHPWaveletFilter->SetInput(highPassWavelets[band]);
        decimateHPWaveletFilter->Update();
        auto changeHPDecimateInfoFilter = ChangeInformationFilterType::New();
        changeHPDecimateInfoFilter->ChangeAll();
        changeHPDecimateInfoFilter->UseReferenceImageOn();
        changeHPDecimateInfoFilter->SetReferenceImage(inputPerLevel);
        changeHPDecimateInfoFilter->SetInput(decimateHPWaveletFilter->GetOutput());
        changeHPDecimateInfoFilter->Update();
        highPassWavelets[band] = changeHPDecimateInfoFilter->GetOutput();
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
