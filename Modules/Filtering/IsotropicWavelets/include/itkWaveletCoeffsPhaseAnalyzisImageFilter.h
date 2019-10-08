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
#ifndef itkWaveletCoeffsPhaseAnalyzisImageFilter_h
#define itkWaveletCoeffsPhaseAnalyzisImageFilter_h

#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkFFTPadImageFilter.h"
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyInverse.h"
#include "itkWaveletFrequencyForwardUndecimated.h"
#include "itkWaveletFrequencyInverseUndecimated.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkMonogenicSignalFrequencyImageFilter.h"
#include "itkVectorInverseFFTImageFilter.h"
#include "itkPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkZeroDCImageFilter.h"
#include "itkImage.h"
#include "itkCastImageFilter.h"
#include "itkNumberToString.h"
#include <string>


namespace itk
{
/** \class WaveletCoeffsPhaseAnalyzis
 * @brief IsotropicWavelet multiscale analysis and reconstructing phase analysis results where input is an image in the
 * spatial domain. Output Layout: Output is a single spatial domain image carrying the reconstructed phase analysis
 * information.
 *
 * @note The information/metadata of input image is ignored.
 * It can be saved on the disk with a @sa ImageFileWriter.
 *
 * \ingroup IsotropicWavelets
 */
template <typename TImageType, typename TWaveletFunction>
class WaveletCoeffsPhaseAnalyzisImageFilter : public ImageToImageFilter<TImageType, TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(WaveletCoeffsPhaseAnalyzisImageFilter);

  /** Standard typenames type alias. */
  using Self = WaveletCoeffsPhaseAnalyzisImageFilter;
  using Superclass = ImageToImageFilter<TImageType, TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ImageType = TImageType;
  using IntType = unsigned int;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  using WaveletScalarType = double;
  using ImageFloatType = Image<float, ImageDimension>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(WaveletCoeffsPhaseAnalyzisImageFilter, ImageToImageFilter);

  /** Flag to store the number of levels, highpasssubbands, output index, applysoftthreshold,
  thresholdnumofsigmas.**/
  itkGetMacro(Levels, IntType);
  itkSetMacro(Levels, IntType);
  itkGetMacro(HighPassSubBands, IntType);
  itkSetMacro(HighPassSubBands, IntType);
  itkGetMacro(OutputIndex, IntType);
  itkSetMacro(OutputIndex, IntType);
  itkGetMacro(ApplySoftThreshold, bool);
  itkSetMacro(ApplySoftThreshold, bool);
  itkGetMacro(ThresholdNumOfSigmas, double);
  itkSetMacro(ThresholdNumOfSigmas, double);

protected:
  WaveletCoeffsPhaseAnalyzisImageFilter();

protected:
  using FFTPadType = FFTPadImageFilter<ImageType>;
  using ZeroDCType = ZeroDCImageFilter<ImageType>;
  using FFTForwardType = ForwardFFTImageFilter<typename ZeroDCType::OutputImageType>;
  using ComplexImageType = typename FFTForwardType::OutputImageType;

  using WaveletFunctionType = SimoncelliIsotropicWavelet<WaveletScalarType, ImageDimension>;
  using WaveletFilterBankType = WaveletFrequencyFilterBankGenerator<ComplexImageType, WaveletFunctionType>;
  using ForwardWaveletType = WaveletFrequencyForward<ComplexImageType, ComplexImageType, WaveletFilterBankType>;

  using MonogenicSignalFrequencyType = MonogenicSignalFrequencyImageFilter<ComplexImageType>;
  using VectorMonoOutputType = typename MonogenicSignalFrequencyType::OutputImageType;
  using VectorInverseFFTType = VectorInverseFFTImageFilter<VectorMonoOutputType>;
  using PhaseAnalysisType = PhaseAnalysisSoftThresholdImageFilter<typename VectorInverseFFTType::OutputImageType>;

  using InverseWaveletType = WaveletFrequencyInverse<ComplexImageType, ComplexImageType, WaveletFilterBankType>;
  using InverseFFTType = InverseFFTImageFilter<ComplexImageType, ImageType>;
  using ChangeInformationType = ChangeInformationImageFilter<ImageType>;
  using CastFloatType = CastImageFilter<ImageType, ImageFloatType>;

  /** Single-threaded version of GenerateData. */
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  typename FFTPadType::Pointer         m_FFTPadFilter;
  typename ZeroDCType::Pointer         m_ZeroDCFilter;
  typename FFTForwardType::Pointer     m_ForwardFFTFilter;
  typename ForwardWaveletType::Pointer m_ForwardWaveletFilter;

  typename MonogenicSignalFrequencyType::Pointer m_MonogenicSignalFrequencyFilter;
  typename VectorInverseFFTType::Pointer         m_VectorInverseFFTFilter;
  typename PhaseAnalysisType::Pointer            m_PhaseAnalysisFilter;
  typename FFTForwardType::Pointer               m_FFTForwardPhaseFilter;

  typename InverseWaveletType::Pointer m_InverseWaveletFilter;
  typename InverseFFTType::Pointer     m_InverseFFTFilter;

  typename ChangeInformationType::Pointer m_ChangeInformationFilter;
  typename CastFloatType::Pointer         m_CastFloatFilter;

  unsigned int m_Levels;
  unsigned int m_HighPassSubBands;
  unsigned int m_OutputIndex;
  bool         m_ApplySoftThreshold;
  double       m_ThresholdNumOfSigmas;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletCoeffsPhaseAnalyzisImageFilter.hxx"
#endif

#endif
