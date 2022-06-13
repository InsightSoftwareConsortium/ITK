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
#ifndef itkIsotropicWaveletFrequencyFunction_h
#define itkIsotropicWaveletFrequencyFunction_h

#include "itkIsotropicFrequencyFunction.h"

namespace itk
{
/** \class IsotropicWaveletFrequencyFunction
 * Abstract class for IsotropicWaveletFrequencyFunction, subclass of
 * \sa IsotropicFrequencyFunction implementing a M-Band decomposition of frequencies.
 * (There are at least one high-pass and one low-pass decomposition)

 * The interface consists on EvaluateAlgo functions.
 * Where Algo can be:
 *  Function: MotherWavelet response.
 *  ForwardYYY: Forward/Analysis filter response.
 *  InverseYYY: Inverse/Synthesis filter response.
 *  Where YYY can be:
 *   LowPassFilter
 *   HighPassFilter
 *   SubBandFilter
 *
 * Note that for Isotropic Wavelet, forward and inverse responses are usually the same.
 *  Data member m_HighPassSubBand refers to the numbers of bands in the highpass filter. Default to 1, just one highpass
 filter, so no band analysis.
 *
 *  If using EvaluateAlgoYYYSubBandFilter(unsigned int k). Implement it like this:
 *  k = 0 return low-pass response.
 *  k = m_HighPassSubBand return high-pass response.
 *  0 < k < m_HighPassSubBand calls the high pass subbands.
 *
 * For an example \sa itkHeldIsotropicWavelet
 *
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = double,
          unsigned int VImageDimension = 3,
          typename TInput = Vector<SpacePrecisionType, VImageDimension>>
class IsotropicWaveletFrequencyFunction : public IsotropicFrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IsotropicWaveletFrequencyFunction);

  /** Standard class type alias. */
  using Self = IsotropicWaveletFrequencyFunction;
  using Superclass = IsotropicFrequencyFunction<TFunctionValue, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(IsotropicWaveletFrequencyFunction, IsotropicFrequencyFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using FunctionValueType = typename Superclass::FunctionValueType;
  using OutputType = typename Superclass::OutputType;

  /** Evaluate Magnitude of frequency point. Evaluate function calls this. */
  FunctionValueType
  EvaluateMagnitude(const TFunctionValue & freq_norm_in_hz) const override = 0;

  /**** Forward/Analysis ***/
  /** Evaluate the low filter response. */
  virtual FunctionValueType
  EvaluateForwardLowPassFilter(const FunctionValueType & freq_in_hz) const;

  /** Evaluate the highfilter response. */
  virtual FunctionValueType
  EvaluateForwardHighPassFilter(const FunctionValueType & freq_in_hz) const;

  /** Evaluate the sub-band response.
   * j evaluates LowFilter, j=m_SubBand evaluates HighFilter */
  virtual FunctionValueType
  EvaluateForwardSubBand(const FunctionValueType & freq_in_hz, unsigned int j) const;

  /**** Inverse/Synthesis ***/
  /** Evaluate the low filter response. */
  virtual FunctionValueType
  EvaluateInverseLowPassFilter(const FunctionValueType & freq_in_hz) const;

  /** Evaluate the highfilter response. */
  virtual FunctionValueType
  EvaluateInverseHighPassFilter(const FunctionValueType & freq_in_hz) const;

  /** Evaluate the sub-band response.
   * j evaluates LowFilter, j=m_SubBand evaluates HighFilter */
  virtual FunctionValueType
  EvaluateInverseSubBand(const FunctionValueType & freq_in_hz, unsigned int j) const;

  /** Number of HighPassSubBands in the high filter decomposition.
   * Default to the minimal value: 1. */
  itkGetConstMacro(HighPassSubBands, unsigned int);
  virtual void
  SetHighPassSubBands(const unsigned int & high_pass_bands);

  /** Cut off frequency for low and high pass frequency.
   * Default to 0.25 Hz ( pi/2 rad/s ) but can be changed in child classes. */
  itkGetConstMacro(FreqCutOff, FunctionValueType);
  // itkSetMacro(FreqCutOff, FunctionValueType);
protected:
  IsotropicWaveletFrequencyFunction();
  ~IsotropicWaveletFrequencyFunction() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  unsigned int      m_HighPassSubBands{ 1 };
  FunctionValueType m_FreqCutOff;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIsotropicWaveletFrequencyFunction.hxx"
#endif

#endif
