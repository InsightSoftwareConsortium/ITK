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
#include <itkFrequencyShrinkViaInverseFFTImageFilter.h>
#include <itkShrinkDecimateImageFilter.h>
#include <itkChangeInformationImageFilter.h>

// Debug TODO:deleteme
//  Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkComplexToRealImageFilter.h"
#  include "itkNumberToString.h"
#  include "../test/itkViewImage.h"
#endif

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
  // if (linear_index == this->m_TotalOutputs - 1 )
  //   return std::make_pair(this->m_Levels, 0);

  unsigned int band = (linear_index) % this->m_HighPassSubBands;
  // note integer division ahead.
  unsigned int level = (linear_index) / this->m_HighPassSubBands;
  itkAssertInDebugAndIgnoreInReleaseMacro(level >= 0);
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
  return this->GetOutput(this->m_TotalOutputs - 1);
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
std::vector<typename WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::OutputImagePointer>
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GetOutputsHighPassByLevel(unsigned int level)
{
  std::vector<OutputImagePointer> outputPtrs;
  unsigned int                    nOutput_start = level * this->m_HighPassSubBands;
  unsigned int                    nOutput_end = (level + 1) * this->m_HighPassSubBands;
  if (nOutput_end > this->m_TotalOutputs)
    nOutput_end = this->m_TotalOutputs;

  for (unsigned int nOutput = nOutput_start; nOutput < nOutput_end; ++nOutput)
  {
    outputPtrs.push_back(this->GetOutput(nOutput));
  }
  return outputPtrs;
}

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
unsigned int
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::ComputeMaxNumberOfLevels(
  typename InputImageType::SizeType & input_size)
{
  FixedArray<unsigned int, ImageDimension> exponent_per_axis;
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
  return *std::min_element(exponent_per_axis.Begin(), exponent_per_axis.End());
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
        continue;
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
        inputSizePerLevel[idim] = 1;
      // Index dividided by scale
      inputStartIndexPerLevel[idim] = static_cast<IndexValueType>(
        std::ceil(static_cast<double>(inputStartIndexPerLevel[idim]) / this->m_ScaleFactor));
      // Spacing
      inputSpacingPerLevel[idim] = inputSpacingPerLevel[idim] * this->m_ScaleFactor;
      // Origin, the same.
      // inputOriginPerLevel[idim] = inputOriginPerLevel[idim] ;
      // inputOriginPerLevel[idim] = inputOriginPerLevel[idim] / this->m_ScaleFactor;
    }

    // Set the low pass at the end.
    if (level == this->m_Levels - 1)
    {
      outputPtr = this->GetOutput(this->m_TotalOutputs - 1);
      if (!outputPtr)
        continue;
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

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateOutputRequestedRegion(
  DataObject * refOutput)
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion(refOutput);

  // find the index for this output
  unsigned int                          refIndex = static_cast<unsigned int>(refOutput->GetSourceOutputIndex());
  std::pair<unsigned int, unsigned int> pairRef = this->OutputIndexToLevelBand(refIndex);
  unsigned int                          refLevel = pairRef.first;
  // unsigned int refBand  = pairRef.second;

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
    RegionType baseRegion = ptr->GetRequestedRegion();
    IndexType  baseIndex = baseRegion.GetIndex();
    SizeType   baseSize = baseRegion.GetSize();

    int distanceToReferenceLevel = -1000;
    for (unsigned int level = 0; level < this->m_Levels + 1; ++level)
    {
      distanceToReferenceLevel = static_cast<int>(refLevel) - static_cast<int>(level);
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
          continue;
        if (!this->GetOutput(n_output))
          continue;
        outputRegion.Crop(this->GetOutput(n_output)->GetLargestPossibleRegion());
        this->GetOutput(n_output)->SetRequestedRegion(outputRegion);
      }
      else // Bands per level
      {
        for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
        {
          unsigned int n_output = level * this->m_HighPassSubBands + band;
          if (n_output == refIndex)
            continue;
          if (!this->GetOutput(n_output))
            continue;
          outputRegion.Crop(this->GetOutput(n_output)->GetLargestPossibleRegion());
          // set the requested region
          this->GetOutput(n_output)->SetRequestedRegion(outputRegion);
        }
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

template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
void
WaveletFrequencyForward<TInputImage, TOutputImage, TWaveletFilterBank>::GenerateData()
{
  InputImageConstPointer input = this->GetInput();
  this->AllocateOutputs();

  typedef itk::CastImageFilter<InputImageType, OutputImageType> CastFilterType;
  typename CastFilterType::Pointer                              castFilter = CastFilterType::New();
  castFilter->SetInput(input);
  castFilter->Update();
  OutputImagePointer                                         inputPerLevel = castFilter->GetOutput();
  typedef itk::ChangeInformationImageFilter<OutputImageType> ChangeInformationFilterType;
  typename ChangeInformationFilterType::Pointer              changeInputInfoFilter = ChangeInformationFilterType::New();
  // TODO solve the origin/spacing issue
  // For multiplication purposes between the filterbank and the inputimage, both images have to have same
  // Information/Metadata. current a) ignore the input information and work with default values. CONS: the user have to
  // restore it explicitly after reconstruction. b) set the information of the filter bank to be the same. CONS: the
  // metadata/information could be misleading working in the frequency space.
  typename InputImageType::PointType   origin_old = inputPerLevel->GetOrigin();
  typename InputImageType::SpacingType spacing_old = inputPerLevel->GetSpacing();
  typename InputImageType::PointType   origin_new = origin_old;
  origin_new.Fill(0);
  typename InputImageType::SpacingType spacing_new = spacing_old;
  spacing_new.Fill(1);
  changeInputInfoFilter->SetInput(inputPerLevel);
  changeInputInfoFilter->ChangeDirectionOff();
  changeInputInfoFilter->ChangeRegionOff();
  changeInputInfoFilter->ChangeSpacingOn();
  changeInputInfoFilter->ChangeOriginOn();
  changeInputInfoFilter->UseReferenceImageOff();
  changeInputInfoFilter->SetOutputOrigin(origin_new);
  changeInputInfoFilter->SetOutputSpacing(spacing_new);
  changeInputInfoFilter->Update();

  // Create Wavelet filter.
  // TODO Phc: Current: Wavelet Filter Bank images always have default frequency range: [0,1/2].
  // This might be wrong.
  // Alternative: Downsample the filter bank as well
  // or change spacing on GenerateSource image of the filter bank, so the iterator see different frequencies.
  /******* Calculate FilterBank with the right size per level. *****/
  // TODO (option b) Set filter bank information to be the same than input image
  // filterBank->SetOrigin(inputPerLevel->GetOrigin() );
  // filterBank->SetSpacing(inputPerLevel->GetSpacing() );
  // filterBank->SetDirection(inputPerLevel->GetDirection() );
  typename WaveletFilterBankType::Pointer filterBank = WaveletFilterBankType::New();
  filterBank->SetHighPassSubBands(this->m_HighPassSubBands);
  filterBank->SetSize(changeInputInfoFilter->GetOutput()->GetLargestPossibleRegion().GetSize());
  filterBank->Update();
  std::vector<OutputImagePointer> highPassWavelets = filterBank->GetOutputsHighPassBands();
  OutputImagePointer              lowPassWavelet = filterBank->GetOutputLowPass();

  // typedef itk::FrequencyShrinkViaInverseFFTImageFilter<OutputImageType> ShrinkFilterType;
  typedef itk::FrequencyShrinkImageFilter<OutputImageType>                 ShrinkFilterType;
  typedef itk::ShrinkDecimateImageFilter<OutputImageType, OutputImageType> ShrinkDecimateFilterType;
  typedef itk::MultiplyImageFilter<OutputImageType>                        MultiplyFilterType;
  inputPerLevel = changeInputInfoFilter->GetOutput();
  for (unsigned int level = 0; level < this->m_Levels; ++level)
  {

    /******* Set HighPass bands *****/
    itkDebugMacro(<< "Number of FilterBank high pass bands: " << highPassWavelets.size());
    for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
    {
      unsigned int n_output = level * this->m_HighPassSubBands + band;
      /******* Band dilation factor for HighPass bands *****/
      //  2^(1/#bands) instead of Dyadic dilations.
      typename MultiplyFilterType::Pointer multiplyByAnalysisBandFactor = MultiplyFilterType::New();
      multiplyByAnalysisBandFactor->SetInput1(highPassWavelets[band]);
      // double expFactorHigh = - static_cast<int>(band + 1)/static_cast<double>(this->m_HighPassSubBands) *
      // static_cast<double>(ImageDimension)/2.0; double expFactorHigh = -
      // static_cast<double>(1)/static_cast<double>(this->m_HighPassSubBands) * static_cast<double>(ImageDimension)/2.0;
      // double expBandFactor = expLevelFactor;// + static_cast<double>(ImageDimension)/2.0;
      // double expBandFactor = expLevelFactor - static_cast<int>(band)/static_cast<double>(this->m_HighPassSubBands) *
      // static_cast<double>(ImageDimension)/2.0; double expBandFactor = 0; double expBandFactor = -
      // static_cast<double>(level*ImageDimension)/2.0;
      double expBandFactor =
        (-static_cast<double>(level) + band / static_cast<double>(this->m_HighPassSubBands)) * ImageDimension / 2.0;
      multiplyByAnalysisBandFactor->SetConstant(std::pow(2.0, expBandFactor));
      // TODO Warning: InPlace here deletes buffered region of input.
      // http://public.kitware.com/pipermail/community/2015-April/008819.html
      // multiplyByAnalysisBandFactor->InPlaceOn();
      multiplyByAnalysisBandFactor->Update();

      typename MultiplyFilterType::Pointer multiplyHighBandFilter = MultiplyFilterType::New();
      multiplyHighBandFilter->SetInput1(multiplyByAnalysisBandFactor->GetOutput());
      multiplyHighBandFilter->SetInput2(inputPerLevel);
      multiplyHighBandFilter->InPlaceOn();
      multiplyHighBandFilter->GraftOutput(this->GetOutput(n_output));
      multiplyHighBandFilter->Update();

      this->UpdateProgress(static_cast<float>(n_output - 1) / static_cast<float>(m_TotalOutputs));
      this->GraftNthOutput(n_output, multiplyHighBandFilter->GetOutput());
    }
    /******* Calculate LowPass band *****/
    typename MultiplyFilterType::Pointer multiplyLowFilter = MultiplyFilterType::New();
    multiplyLowFilter->SetInput1(lowPassWavelet);
    multiplyLowFilter->SetInput2(inputPerLevel);
    // multiplyLowFilter->InPlaceOn();
    multiplyLowFilter->Update();
    // Store result without dilation factor for next level.
    inputPerLevel = multiplyLowFilter->GetOutput();

    /* Shrink in the frequency domain the
     * stored low band for the next level */
    typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetInput(inputPerLevel);
    shrinkFilter->SetShrinkFactors(this->m_ScaleFactor);
    shrinkFilter->Update();

    if (level == this->m_Levels - 1) // Set low_pass output (index=this->m_TotalOutputs - 1)
    {
      // Apply dilation factor on low band only in the last level.
      // double expLevelFactor = - static_cast<double>( (level + 1) * ImageDimension ) / 2.0;
      // double expLevelFactor = - static_cast<double>( level * ImageDimension ) / 2.0;
      double expLevelFactor = 0;
      itkDebugMacro(<< "ExpLevelFactor: " << expLevelFactor << ", level: " << level
                    << " 2^expLevelFactor: " << std::pow(2.0, expLevelFactor));
      typename MultiplyFilterType::Pointer multiplyByDilationLevelFactor = MultiplyFilterType::New();
      multiplyByDilationLevelFactor->SetInput1(shrinkFilter->GetOutput());
      multiplyByDilationLevelFactor->SetConstant(std::pow(2.0, expLevelFactor));
      multiplyByDilationLevelFactor->InPlaceOn();
      multiplyByDilationLevelFactor->GraftOutput(this->GetOutput(this->m_TotalOutputs - 1));
      multiplyByDilationLevelFactor->Update();
      this->UpdateProgress(static_cast<float>(this->m_TotalOutputs - 1) / static_cast<float>(this->m_TotalOutputs));
      this->GraftNthOutput(this->m_TotalOutputs - 1, multiplyByDilationLevelFactor->GetOutput());
      continue;
    }
    else // update inputPerLevel
    {
      // #ifdef ITK_VISUALIZE_TESTS
      //       // TODO deleteme after debug
      //       itk::NumberToString< unsigned int > n2s;
      //       typedef double                                  PixelType;
      //       typedef itk::Image< PixelType, ImageDimension > RealImageType;
      //       typedef itk::ComplexToRealImageFilter< OutputImageType, RealImageType > ComplexToRealFilter;
      //       typename ComplexToRealFilter::Pointer complexToRealFilter = ComplexToRealFilter::New();
      // #endif
      inputPerLevel = shrinkFilter->GetOutput();
      /******* DownSample wavelets *****/
      typename ShrinkDecimateFilterType::Pointer decimateWaveletFilter = ShrinkDecimateFilterType::New();
      decimateWaveletFilter->SetInput(lowPassWavelet);
      decimateWaveletFilter->SetShrinkFactors(this->m_ScaleFactor);
      decimateWaveletFilter->Update();
      typename ChangeInformationFilterType::Pointer changeDecimateInfoFilter = ChangeInformationFilterType::New();
      changeDecimateInfoFilter->SetInput(decimateWaveletFilter->GetOutput());
      changeDecimateInfoFilter->ChangeAll();
      changeDecimateInfoFilter->UseReferenceImageOn();
      changeDecimateInfoFilter->SetReferenceImage(inputPerLevel);
      changeDecimateInfoFilter->Update();
      lowPassWavelet = changeDecimateInfoFilter->GetOutput();
      lowPassWavelet->DisconnectPipeline();
      for (unsigned int band = 0; band < this->m_HighPassSubBands; ++band)
      {
        typename ShrinkDecimateFilterType::Pointer decimateHPWaveletFilter = ShrinkDecimateFilterType::New();
        decimateHPWaveletFilter->SetShrinkFactors(this->m_ScaleFactor);
        decimateHPWaveletFilter->SetInput(highPassWavelets[band]);
        decimateHPWaveletFilter->Update();
        typename ChangeInformationFilterType::Pointer changeHPDecimateInfoFilter = ChangeInformationFilterType::New();
        changeHPDecimateInfoFilter->ChangeAll();
        changeHPDecimateInfoFilter->UseReferenceImageOn();
        changeHPDecimateInfoFilter->SetReferenceImage(inputPerLevel);
        changeHPDecimateInfoFilter->SetInput(decimateHPWaveletFilter->GetOutput());
        changeHPDecimateInfoFilter->Update();
        highPassWavelets[band] = changeHPDecimateInfoFilter->GetOutput();
        highPassWavelets[band]->DisconnectPipeline();
      }
    }
  }
}
} // end namespace itk
#endif
