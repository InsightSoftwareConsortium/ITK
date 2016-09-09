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
#ifndef itkWaveletFrequencyForward_hxx
#define itkWaveletFrequencyForward_hxx
#include <itkWaveletFrequencyForward.h>
#include <itkCastImageFilter.h>
#include <itkImage.h>
#include <algorithm>
#include <itkMultiplyImageFilter.h>
#include <itkFrequencyShrinkImageFilter.h>
#include <itkChangeInformationImageFilter.h>
namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::WaveletFrequencyForward()
  : m_Levels(1)
  , m_HighPassSubBands(1)
  , m_TotalOutputs(1)
  , m_ScaleFactor(2)
{
  this->SetNumberOfRequiredInputs(1);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
std::pair<unsigned int, unsigned int>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::OutputIndexToLevelBand(
  unsigned int linear_index)
{
  if (linear_index > this->m_TotalOutputs - 1 || linear_index < 0)
    itkExceptionMacro(<< "Failed converting liner index " << linear_index << " to Level,Band pair : out of bounds");
  // Low pass (band = 0).
  if (linear_index == 0)
    return std::make_pair(this->m_Levels, 0);

  unsigned int band = (linear_index - 1) % this->m_HighPassSubBands;
  band = band + 1;
  // note integer division ahead.
  unsigned int level = (linear_index - band) / this->m_HighPassSubBands + 1;
  assert(level > 0);
  return std::make_pair(level, band);
};

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
std::vector<typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::OutputImagePointer>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputs()
{
  std::vector<OutputImagePointer> outputPtrs;
  for (unsigned int nout = 0; nout < this->m_TotalOutputs; ++nout)
  {
    outputPtrs.push_back(this->GetOutput(nout));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
std::vector<typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::OutputImagePointer>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputsHighPass()
{
  std::vector<OutputImagePointer> outputPtrs;
  for (unsigned int nout = 1; nout < this->m_TotalOutputs; ++nout)
  {
    outputPtrs.push_back(this->GetOutput(nout));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::OutputImagePointer
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputLowPass()
{
  return this->GetOutput(0);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
std::vector<typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::OutputImagePointer>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputsHighPassByLevel(unsigned int level)
{
  std::vector<OutputImagePointer> outputPtrs;
  unsigned int                    nout_start = 1 + level * this->m_HighPassSubBands;
  unsigned int                    nout_end = 1 + (level + 1) * this->m_HighPassSubBands;
  if (nout_end > this->m_TotalOutputs)
    nout_end = this->m_TotalOutputs;

  for (unsigned int nout = nout_start; nout < nout_end; ++nout)
  {
    outputPtrs.push_back(this->GetOutput(nout));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
unsigned int
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::ComputeMaxNumberOfLevels(
  typename InputImageType::SizeType & input_size)
{
  std::array<unsigned int, ImageDimension> exponent_per_axis;
  for (unsigned int axis = 0; axis < ImageDimension; ++axis)
  {
    size_t size_axis = input_size[axis];
    if (size_axis < 2)
    {
      exponent_per_axis[axis] = 1;
      continue;
    }
    double exponent = std::log(size_axis) / std::log(2.0);
    // check that exponent is integer: the fractional part is 0
    double int_part;
    if (std::modf(exponent, &int_part) == 0)
    {
      exponent_per_axis[axis] = static_cast<unsigned int>(exponent);
    }
    else
    {
      exponent_per_axis[axis] = 1;
    }
  }
  // return the min_element of array (1 if any size is not power of 2)
  return *std::min_element(exponent_per_axis.begin(), exponent_per_axis.end());
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::SetLevels(unsigned int n)
{
  unsigned int current_outputs = 1 + this->m_Levels * this->m_HighPassSubBands;
  if (this->m_TotalOutputs == current_outputs && this->m_Levels == n)
  {
    return;
  }

  this->m_Levels = n;
  this->m_TotalOutputs = 1 + n * this->m_HighPassSubBands;

  this->SetNumberOfRequiredOutputs(this->m_TotalOutputs);
  this->Modified();

  for (unsigned int n_output = 0; n_output < this->m_TotalOutputs; ++n_output)
  {
    this->SetNthOutput(n_output, this->MakeOutput(n_output));
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::SetHighPassSubBands(unsigned int k)
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
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::PrintSelf(std::ostream & os,
                                                                                  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << " Levels: " << this->m_Levels << " HighPassSubBands: " << this->m_HighPassSubBands
     << " TotalOutputs: " << this->m_TotalOutputs << std::endl;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer inputPtr = this->GetInput();

  if (!inputPtr)
    itkExceptionMacro(<< "Input has not been set");

  typename InputImageType::PointType     inputOrigin = inputPtr->GetOrigin();
  typename InputImageType::SpacingType   inputSpacing = inputPtr->GetSpacing();
  typename InputImageType::DirectionType inputDirection = inputPtr->GetDirection();
  typename InputImageType::SizeType      inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  typename InputImageType::IndexType     inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  OutputImagePointer                      outputPtr;
  typename OutputImageType::PointType     low_passOrigin;
  typename OutputImageType::SpacingType   low_passSpacing;
  typename OutputImageType::SizeType      low_passSize;
  typename OutputImageType::IndexType     low_passStartIndex;
  typename OutputImageType::DirectionType low_passDirection = inputDirection;

  // we need to compute the output spacing, the output image size,
  // and the output image start index

  for (unsigned int level = 0; level < this->m_Levels; ++level)
  {
    // Bands per level . No downsampling.
    for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
    {
      // current_output = 0 is the low_pass, we deal with it at the end of the pyramid
      unsigned int current_output = 1 + level * m_HighPassSubBands + band;
      // Set to the output
      outputPtr = this->GetOutput(current_output);
      if (!outputPtr)
        continue;
      typename OutputImageType::RegionType largestPossibleRegion;
      largestPossibleRegion.SetSize(inputSize);
      largestPossibleRegion.SetIndex(inputStartIndex);

      outputPtr->SetLargestPossibleRegion(largestPossibleRegion);
      outputPtr->SetOrigin(inputOrigin);
      outputPtr->SetSpacing(inputSpacing);
      outputPtr->SetDirection(inputDirection);
    }

    // Calculate size of low_pass per level
    // unsigned int scaleFactorPerLevel = std::pow(this->m_ScaleFactor, level + 1 );
    // Calculate new Size and Index, per dim.
    for (unsigned int idim = 0; idim < OutputImageType::ImageDimension; idim++)
    {
      // Size divided by scale
      low_passSize[idim] =
        static_cast<SizeValueType>(std::floor(static_cast<double>(inputSize[idim]) / this->m_ScaleFactor));
      if (low_passSize[idim] < 1)
        low_passSize[idim] = 1;
      // Index dividided by scale
      low_passStartIndex[idim] =
        static_cast<IndexValueType>(std::ceil(static_cast<double>(inputStartIndex[idim]) / this->m_ScaleFactor));
      // Spacing
      low_passSpacing[idim] = inputSpacing[idim];
      // Origin.
      low_passOrigin[idim] = inputOrigin[idim];
    }

    // Update InputSize with low_passSize.
    inputSize = low_passSize;
    inputOrigin = low_passOrigin;
    inputSpacing = low_passSpacing;
    // inputDirection = low_passDirection;

    if (level == this->m_Levels - 1)
    {
      outputPtr = this->GetOutput(0);
      if (!outputPtr)
        continue;
      typename OutputImageType::RegionType largestPossibleRegion;
      largestPossibleRegion.SetSize(low_passSize);
      largestPossibleRegion.SetIndex(low_passStartIndex);
      outputPtr->SetLargestPossibleRegion(largestPossibleRegion);
      outputPtr->SetOrigin(low_passOrigin);
      outputPtr->SetSpacing(low_passSpacing);
      outputPtr->SetDirection(low_passDirection);
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateOutputRequestedRegion(
  DataObject * refOutput)
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion(refOutput);

  // find the index for this output
  unsigned int refIndex = static_cast<unsigned int>(refOutput->GetSourceOutputIndex());
  unsigned int refLevel, refBand;
  std::tie(refLevel, refBand) = this->OutputIndexToLevelBand(refIndex);

  // compute baseIndex and baseSize
  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::RegionType RegionType;

  TOutputImage * ptr = itkDynamicCastInDebugMode<TOutputImage *>(refOutput);
  if (!ptr)
    itkExceptionMacro(<< "Could not cast refOutput to TOutputImage*.");

  if (ptr->GetRequestedRegion() == ptr->GetLargestPossibleRegion())
  {
    // set the requested regions for the other outputs to their largest

    for (unsigned int nout = 0; nout < this->m_TotalOutputs; ++nout)
    {
      if (nout == refIndex)
        continue;
      if (!this->GetOutput(nout))
        continue;
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
    IndexType  baseIndex = ptr->GetRequestedRegion().GetIndex();
    SizeType   baseSize = ptr->GetRequestedRegion().GetSize();
    RegionType baseRegion;

    unsigned int exponent_factor = refLevel - 1;
    // if (refIndex != 0)
    //   exponent_factor = refLevel;
    // else
    //   exponent_factor = refLevel - 1;

    for (unsigned int idim = 0; idim < TOutputImage::ImageDimension; idim++)
    {
      baseIndex[idim] *= static_cast<IndexValueType>(std::pow(this->m_ScaleFactor, exponent_factor));
      baseSize[idim] *= static_cast<SizeValueType>(std::pow(this->m_ScaleFactor, exponent_factor));
    }
    baseRegion.SetIndex(baseIndex);
    baseRegion.SetSize(baseSize);

    for (unsigned int level = 0; level < this->m_Levels; ++level)
    {
      // Bands per level
      for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
      {
        unsigned int n_output = 1 + level * this->m_HighPassSubBands + band;
        if (n_output == refIndex)
          continue;
        if (!this->GetOutput(n_output))
          continue;
        outputRegion = baseRegion;
        outputRegion.Crop(this->GetOutput(n_output)->GetLargestPossibleRegion());
        // set the requested region
        this->GetOutput(n_output)->SetRequestedRegion(outputRegion);
      }
      unsigned int scaleFactorPerLevel = std::pow(this->m_ScaleFactor, level + 1);
      // Update baseRegion size and index for low_pass
      for (unsigned int idim = 0; idim < TOutputImage::ImageDimension; idim++)
      {
        // Index by half.
        outputIndex[idim] =
          static_cast<IndexValueType>(std::ceil(static_cast<double>(baseIndex[idim]) / scaleFactorPerLevel));
        // Size by half
        outputSize[idim] =
          static_cast<SizeValueType>(std::floor(static_cast<double>(baseSize[idim]) / scaleFactorPerLevel));
        if (outputSize[idim] < 1)
          itkExceptionMacro(
            << "Failure at level: " << level
            << " in forward wavelet, going to negative image size. Too many levels for input image size.")
      }

      // Update base size;
      baseIndex = outputIndex;
      baseSize = outputSize;
      baseRegion.SetIndex(baseIndex);
      baseRegion.SetSize(baseSize);

      // Set low pass output
      if (level == this->m_Levels - 1)
      {
        unsigned int n_output = 0;
        if (n_output == refIndex)
          continue;
        if (!this->GetOutput(n_output))
          continue;
        outputRegion.Crop(this->GetOutput(n_output)->GetLargestPossibleRegion());
        this->GetOutput(n_output)->SetRequestedRegion(baseRegion);
      }
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateInputRequestedRegion()
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
  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::RegionType RegionType;

  // The first level has the same size than input.
  // At least one band is ensured to exist, so use it.
  unsigned int refOutput = 1;
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

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateData()
{
  InputImageConstPointer input = this->GetInput();
  this->AllocateOutputs();

  std::vector<OutputImagePointer>   outputList;
  std::vector<OutputRegionIterator> outputItList;


  typedef itk::CastImageFilter<InputImageType, OutputImageType> CastFilterType;
  typename CastFilterType::Pointer                              castFilter = CastFilterType::New();
  castFilter->SetInput(input);
  castFilter->Update();
  OutputImagePointer inputPerLevel = castFilter->GetOutput();
  for (unsigned int level = 0; level < this->m_Levels; ++level)
  {
    /******* Calculate FilterBank with the right size per level. *****/
    typedef itk::MultiplyImageFilter<OutputImageType> MultiplyFilterType;
    typename WaveletFilterBankType::Pointer           filterBank = WaveletFilterBankType::New();
    filterBank->SetHighPassSubBands(this->m_HighPassSubBands);
    filterBank->SetSize(inputPerLevel->GetLargestPossibleRegion().GetSize());
    filterBank->Update();

    /******* set HighPass bands *****/
    std::vector<OutputImagePointer> highPassImages = filterBank->GetOutputsHighPassBands();
    for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
    {
      unsigned int n_output = 1 + level * this->m_HighPassSubBands + band;

      typename MultiplyFilterType::Pointer multiplyHighBandFilter = MultiplyFilterType::New();
      multiplyHighBandFilter->SetInput1(highPassImages[band]);
      multiplyHighBandFilter->SetInput2(inputPerLevel);
      multiplyHighBandFilter->InPlaceOn();
      multiplyHighBandFilter->GraftOutput(this->GetOutput(n_output));
      // multiplyHighBandFilter->Modified();
      multiplyHighBandFilter->Update();
      this->UpdateProgress(static_cast<float>(n_output - 1) / static_cast<float>(m_TotalOutputs));
      this->GraftNthOutput(n_output, multiplyHighBandFilter->GetOutput());
    }
    /******* Calculate LowPass band *****/
    typename MultiplyFilterType::Pointer multiplyLowFilter = MultiplyFilterType::New();
    multiplyLowFilter->SetInput1(filterBank->GetOutputLowPass());
    multiplyLowFilter->SetInput2(inputPerLevel);
    multiplyLowFilter->InPlaceOn();
    multiplyLowFilter->Update();
    inputPerLevel = multiplyLowFilter->GetOutput();

    /******* DownSample for the next Level iteration *****/
    // typedef itk::BinShrinkImageFilter<OutputImageType,OutputImageType> ShrinkFilterType;
    // typedef itk::ShrinkImageFilter<OutputImageType,OutputImageType> ShrinkFilterType;
    typedef itk::FrequencyShrinkImageFilter<OutputImageType> ShrinkFilterType;
    typename ShrinkFilterType::Pointer                       shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetInput(inputPerLevel);
    shrinkFilter->SetShrinkFactors(2);
    shrinkFilter->Update();
    // inputPerLevel = shrinkFilter->GetOutput();
    // Ignore modifications of origin and spacing of shrink filters.
    typedef itk::ChangeInformationImageFilter<OutputImageType> ChangeInformationFilterType;
    typename ChangeInformationFilterType::Pointer              changeInfoFilter = ChangeInformationFilterType::New();
    changeInfoFilter->SetInput(shrinkFilter->GetOutput());
    changeInfoFilter->ChangeDirectionOff();
    changeInfoFilter->ChangeRegionOff();
    changeInfoFilter->ChangeSpacingOn();
    changeInfoFilter->ChangeOriginOn();
    changeInfoFilter->UseReferenceImageOn();
    changeInfoFilter->SetReferenceImage(inputPerLevel.GetPointer()); // Use input image as reference.
    if (level == this->m_Levels - 1)                                 // Set low_pass output (index=0)
    {
      changeInfoFilter->GraftOutput(this->GetOutput(0));
      changeInfoFilter->Update();
      this->UpdateProgress(static_cast<float>(m_TotalOutputs - 1) / static_cast<float>(m_TotalOutputs));
      this->GraftNthOutput(0, changeInfoFilter->GetOutput());
      continue;
    }
    else // update inputPerLevel
    {
      changeInfoFilter->Update();
      inputPerLevel = changeInfoFilter->GetOutput();
    }
  }
}

} // end namespace itk
#endif
