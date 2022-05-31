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
#ifndef itkWaveletFrequencyInverseUndecimated_hxx
#define itkWaveletFrequencyInverseUndecimated_hxx
#include <itkCastImageFilter.h>
#include <itkImage.h>
#include <algorithm>
#include <itkMultiplyImageFilter.h>
#include <itkAddImageFilter.h>
#include <itkImageDuplicator.h>
#include <itkChangeInformationImageFilter.h>
#include <itkWaveletUtilities.h>
namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::WaveletFrequencyInverseUndecimated()


{
  this->SetNumberOfRequiredOutputs(1);
  this->m_WaveletFilterBank = WaveletFilterBankType::New();
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
std::pair<unsigned int, unsigned int>
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::InputIndexToLevelBand(
  unsigned int linear_index)
{
  return itk::utils::IndexToLevelBandSteerablePyramid(linear_index, this->m_Levels, this->m_HighPassSubBands);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::SetLevels(unsigned int n)
{
  unsigned int current_inputs = 1 + this->m_Levels * this->m_HighPassSubBands;

  if (this->m_TotalInputs == current_inputs && this->m_Levels == n)
  {
    return;
  }

  this->m_Levels = n;
  this->m_TotalInputs = 1 + n * this->m_HighPassSubBands;

  this->SetNumberOfRequiredInputs(this->m_TotalInputs);
  this->Modified();

  this->SetNthOutput(0, this->MakeOutput(0));
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::SetHighPassSubBands(unsigned int k)
{
  if (this->m_HighPassSubBands == k)
  {
    return;
  }
  this->m_HighPassSubBands = k;
  // Trigger setting new number of inputs avoiding code duplication
  this->SetLevels(this->m_Levels);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::SetInputs(const InputsType & inputs)
{
  if (inputs.size() != this->m_TotalInputs)
  {
    itkExceptionMacro(<< "Error setting inputs in inverse wavelet. Wrong vector size: " << inputs.size()
                      << " .According to number of levels and bands it should be: " << m_TotalInputs);
  }
  for (unsigned int nin = 0; nin < this->m_TotalInputs; ++nin)
  {
    if (this->GetInput(nin) != inputs[nin])
    {
      this->SetNthInput(nin, inputs[nin]);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::SetInputLowPass(
  const InputImagePointer & input_low_pass)
{
  this->SetNthInput(this->m_TotalInputs - 1, input_low_pass);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::SetInputsHighPass(
  const InputsType & inputs)
{
  if (inputs.size() != this->m_TotalInputs - 1)
  {
    itkExceptionMacro(<< "Error setting inputs in inverse wavelet. Wrong vector size: " << inputs.size()
                      << " .According to number of levels and bands it should be: " << m_TotalInputs - 1);
  }
  for (unsigned int nin = 0; nin < this->m_TotalInputs - 1; ++nin)
  {
    this->SetNthInput(nin, inputs[nin]);
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::PrintSelf(std::ostream & os,
                                                                                             Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Levels: " << this->m_Levels << std::endl;
  os << indent << "HighPassSubBands: " << this->m_HighPassSubBands << std::endl;
  os << indent << "TotalInputs: " << this->m_TotalInputs << std::endl;
  os << indent << "ScaleFactor: " << this->m_ScaleFactor << std::endl;
  os << indent << "ApplyReconstructionFactors: " << this->m_ApplyReconstructionFactors << std::endl;
  os << indent << "UseWaveletFilterBankPyramid: " << this->m_UseWaveletFilterBankPyramid << std::endl;
  itkPrintSelfObjectMacro(WaveletFilterBank);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();
  // Check  all inputs exist.
  for (unsigned int nInput = 0; nInput < this->m_TotalInputs; ++nInput)
  {
    if (!this->GetInput(nInput))
    {
      itkExceptionMacro(<< "Input: " << nInput << " has not been set");
    }
  }

  // We know inputIndex = 0 has the same size than output. Use it.
  InputImagePointer                              inputPtr = const_cast<InputImageType *>(this->GetInput(0));
  const typename InputImageType::PointType &     inputOrigin = inputPtr->GetOrigin();
  const typename InputImageType::SpacingType &   inputSpacing = inputPtr->GetSpacing();
  const typename InputImageType::DirectionType & inputDirection = inputPtr->GetDirection();
  const typename InputImageType::SizeType &      inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType &     inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  OutputImagePointer outputPtr;
  outputPtr = this->GetOutput(0);

  typename OutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(inputSize);
  outputLargestPossibleRegion.SetIndex(inputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
  outputPtr->SetOrigin(inputOrigin);
  outputPtr->SetSpacing(inputSpacing);
  outputPtr->SetDirection(inputDirection);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateOutputRequestedRegion(
  DataObject * refOutput)
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion(refOutput);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // compute baseIndex and baseSize
  using SizeType = typename OutputImageType::SizeType;
  using IndexType = typename OutputImageType::IndexType;
  using RegionType = typename OutputImageType::RegionType;

  OutputImagePointer outputPtr = this->GetOutput(0);

  SizeType   baseSize = outputPtr->GetRequestedRegion().GetSize();
  IndexType  baseIndex = outputPtr->GetRequestedRegion().GetIndex();
  RegionType baseRegion;
  baseRegion.SetIndex(baseIndex);
  baseRegion.SetSize(baseSize);
  for (unsigned int level = 0; level < this->m_Levels; ++level)
  {
    for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
    {
      unsigned int nInput = level * this->m_HighPassSubBands + band;
      if (!this->GetInput(nInput))
      {
        itkExceptionMacro(<< "Input ptr does not exist: " << nInput);
      }
      InputImagePointer inputPtr = const_cast<InputImageType *>(this->GetInput(nInput));
      // set the requested region
      inputPtr->SetRequestedRegion(baseRegion);
    }
  }
}

// ITK forward implementation: Freq Domain, no downsampling
//    - HPs (lv1 wavelet coef)
// I -         - HPs (lv2 wavelet coef)
//    - LP  -
//             - LP
// Where Down is a downsample.
template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyInverseUndecimated<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateData()
{
  this->AllocateOutputs();
  // Start with the approximation image (the smallest).
  InputImageConstPointer low_pass = this->GetInput(this->m_TotalInputs - 1);

  using DuplicatorType = itk::ImageDuplicator<InputImageType>;
  auto duplicator = DuplicatorType::New();
  duplicator->SetInputImage(low_pass);
  duplicator->Update();
  InputImagePointer low_pass_per_level = duplicator->GetOutput();

  using MultiplyFilterType = itk::MultiplyImageFilter<InputImageType>;
  auto scaleFactor = static_cast<double>(this->m_ScaleFactor);
  for (int level = this->m_Levels - 1; level > -1; --level)
  {
    itkDebugMacro(<< "LEVEL: " << level);
    itkDebugMacro(<< "Low_pass_per_level: " << level << " Region:" << low_pass_per_level->GetLargestPossibleRegion());

    /******* Calculate FilterBank with the right size per level. *****/
    // Save the FilterBank vector created in the forward wavelet and load it here to save compute it again.
    InputImagePointer waveletLow;
    if (!this->m_UseWaveletFilterBankPyramid)
    {
      this->m_WaveletFilterBank->SetHighPassSubBands(this->m_HighPassSubBands);
      this->m_WaveletFilterBank->SetSize(low_pass_per_level->GetLargestPossibleRegion().GetSize());
      this->m_WaveletFilterBank->SetInverseBank(true);
      // this->m_WaveletFilterBank->SetScaleFactor(this->m_ScaleFactor);
      this->m_WaveletFilterBank->SetLevel(level);
      this->m_WaveletFilterBank->Modified();
      this->m_WaveletFilterBank->UpdateLargestPossibleRegion();
      waveletLow = this->m_WaveletFilterBank->GetOutputLowPass();
    }
    else
    {
      waveletLow = this->m_WaveletFilterBankPyramid[level * (1 + this->m_HighPassSubBands)];
    }
    itkDebugMacro(<< "waveletLow: " << level << " Region:" << waveletLow->GetLargestPossibleRegion());
    using ChangeInformationFilterType = itk::ChangeInformationImageFilter<InputImageType>;

    /******* LowPass band *****/
    auto multiplyLowPass = MultiplyFilterType::New();
    multiplyLowPass->SetInput1(waveletLow);
    multiplyLowPass->SetInput2(low_pass_per_level);
    multiplyLowPass->Update();

    low_pass_per_level = multiplyLowPass->GetOutput();

    /******* HighPass sub-bands *****/
    InputsType highPassMasks;
    if (!this->m_UseWaveletFilterBankPyramid)
    {
      highPassMasks = this->m_WaveletFilterBank->GetOutputsHighPassBands();
    }
    else
    {
      highPassMasks.insert(highPassMasks.begin(),
                           this->m_WaveletFilterBankPyramid.begin() + 1 + level * (1 + this->m_HighPassSubBands),
                           this->m_WaveletFilterBankPyramid.begin() + this->m_HighPassSubBands + 1 +
                             level * (1 + this->m_HighPassSubBands));
    }
    // Store HighBands steps into high_pass_reconstruction image:
    InputImagePointer reconstructed = InputImageType::New();
    reconstructed->SetRegions(low_pass_per_level->GetLargestPossibleRegion());
    reconstructed->Allocate();
    reconstructed->FillBuffer(0);
    InputImagePointer bandInputImage;
    for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
    {
      unsigned int nInput = level * this->m_HighPassSubBands + band;
      bandInputImage = const_cast<InputImageType *>(this->GetInput(nInput));
      reconstructed->SetSpacing(bandInputImage->GetSpacing());
      reconstructed->SetOrigin(bandInputImage->GetOrigin());
      reconstructed->SetDirection(bandInputImage->GetDirection());

      auto changeWaveletHighInfoFilter = ChangeInformationFilterType::New();
      changeWaveletHighInfoFilter->SetInput(highPassMasks[band]);
      changeWaveletHighInfoFilter->UseReferenceImageOn();
      changeWaveletHighInfoFilter->SetReferenceImage(bandInputImage);
      changeWaveletHighInfoFilter->ChangeRegionOff();
      changeWaveletHighInfoFilter->ChangeDirectionOn();
      changeWaveletHighInfoFilter->ChangeSpacingOn();
      changeWaveletHighInfoFilter->ChangeOriginOn();
      changeWaveletHighInfoFilter->UpdateLargestPossibleRegion();
      highPassMasks[band] = changeWaveletHighInfoFilter->GetOutput();
      highPassMasks[band]->DisconnectPipeline();

      auto multiplyHighBandFilter = MultiplyFilterType::New();
      multiplyHighBandFilter->SetInput1(highPassMasks[band]);
      multiplyHighBandFilter->SetInput2(bandInputImage);
      multiplyHighBandFilter->UpdateLargestPossibleRegion();

      /******* Band dilation factor for HighPass bands *****/
      //  2^(1/#bands) instead of Dyadic dilations.
      auto multiplyByReconstructionBandFactor = MultiplyFilterType::New();
      multiplyByReconstructionBandFactor->SetInput1(multiplyHighBandFilter->GetOutput());
      double expBandFactor = 0;
      if (this->GetApplyReconstructionFactors())
      {
        expBandFactor = -(band / static_cast<double>(this->m_HighPassSubBands)) * ImageDimension / 2.0;
      }
      multiplyByReconstructionBandFactor->SetConstant(std::pow(scaleFactor, expBandFactor));
      multiplyByReconstructionBandFactor->InPlaceOn();
      multiplyByReconstructionBandFactor->Update();

      /******* Add high bands *****/
      using AddFilterType = itk::AddImageFilter<InputImageType>;
      auto addFilter = AddFilterType::New();
      addFilter->SetInput1(reconstructed);
      addFilter->SetInput2(multiplyByReconstructionBandFactor->GetOutput());
      addFilter->InPlaceOn();
      addFilter->Update();
      reconstructed = addFilter->GetOutput();

      this->UpdateProgress(static_cast<float>(m_TotalInputs - nInput - 1) / static_cast<float>(m_TotalInputs));
    }

    /******* Add low pass to the sum of high pass bands. *****/
    using AddFilterType = itk::AddImageFilter<InputImageType>;
    auto addHighAndLow = AddFilterType::New();
    addHighAndLow->SetInput1(reconstructed.GetPointer()); // HighBands
    addHighAndLow->SetInput2(low_pass_per_level);
    addHighAndLow->InPlaceOn();
    addHighAndLow->Update();

    // Dilation factor for reconstructed by one level.
    auto multiplyByLevelFactor = MultiplyFilterType::New();
    multiplyByLevelFactor->SetInput1(addHighAndLow->GetOutput());
    double expLevelFactor = 0;
    if (this->GetApplyReconstructionFactors())
    {
      expLevelFactor = static_cast<double>(ImageDimension) / 2.0;
    }
    multiplyByLevelFactor->SetConstant(std::pow(scaleFactor, expLevelFactor));
    multiplyByLevelFactor->InPlaceOn();
    multiplyByLevelFactor->Update();

    if (level == 0 /* Last level to compute */) // Graft Output
    {
      using CastFilterType = itk::CastImageFilter<InputImageType, OutputImageType>;
      auto castFilter = CastFilterType::New();
      castFilter->SetInput(multiplyByLevelFactor->GetOutput());
      castFilter->GraftOutput(this->GetOutput());
      castFilter->Update();
      this->GraftOutput(castFilter->GetOutput());
    }
    else // Update low_pass
    {
      low_pass_per_level = multiplyByLevelFactor->GetOutput();
    }
  }
}
} // end namespace itk
#endif
