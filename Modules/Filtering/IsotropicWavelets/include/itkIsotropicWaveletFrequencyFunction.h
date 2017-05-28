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
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class IsotropicWaveletFrequencyFunction : public IsotropicFrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef IsotropicWaveletFrequencyFunction                                   Self;
  typedef IsotropicFrequencyFunction<TFunctionValue, VImageDimension, TInput> Superclass;
  typedef SmartPointer<Self>                                                  Pointer;
  typedef SmartPointer<const Self>                                            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(IsotropicWaveletFrequencyFunction, IsotropicFrequencyFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::FunctionValueType FunctionValueType;
  typedef typename Superclass::OutputType        OutputType;

  /** Evaluate Magnitude of frequency point. Evaluate function calls this. */
  virtual FunctionValueType
  EvaluateMagnitude(const TFunctionValue & freq_norm_in_hz) const ITK_OVERRIDE = 0;

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
  virtual ~IsotropicWaveletFrequencyFunction();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  unsigned int      m_HighPassSubBands;
  FunctionValueType m_FreqCutOff;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IsotropicWaveletFrequencyFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIsotropicWaveletFrequencyFunction.hxx"
#endif

#endif
