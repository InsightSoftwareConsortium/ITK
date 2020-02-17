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
#ifndef itkIsotropicWaveletFrequencyFunction_hxx
#define itkIsotropicWaveletFrequencyFunction_hxx

#include "itkIsotropicWaveletFrequencyFunction.h"
#include <cmath>
#include <itkMath.h>

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::IsotropicWaveletFrequencyFunction()
  : m_HighPassSubBands(1)
  , m_FreqCutOff(0.25)
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::~IsotropicWaveletFrequencyFunction() =
  default;

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os,
                                                                                      Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_HighPassSubBands: " << m_HighPassSubBands << std::endl;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::SetHighPassSubBands(
  const unsigned int & high_pass_bands)
{
  if (high_pass_bands == 0)
  {
    itkExceptionMacro("HighPassSubands must be greater than zero. At least one high band must exist.");
  }
  this->m_HighPassSubBands = high_pass_bands;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::FunctionValueType
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateForwardLowPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  FunctionValueType value = std::pow(freq_norm_in_hz, static_cast<int>(this->m_HighPassSubBands)) *
                            std::pow(2.0, static_cast<int>(2 * this->m_HighPassSubBands - 1));

  if (value > this->m_FreqCutOff)
  {
    return this->EvaluateMagnitude(value);
  }
  return 1;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::FunctionValueType
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateForwardHighPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  FunctionValueType value = std::pow(freq_norm_in_hz, static_cast<int>(this->m_HighPassSubBands)) *
                            std::pow(2.0, static_cast<int>(this->m_HighPassSubBands - 1));

  if (value < this->m_FreqCutOff)
  {
    return this->EvaluateMagnitude(value);
  }
  return 1;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::FunctionValueType
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateForwardSubBand(
  const FunctionValueType & freq_norm_in_hz,
  unsigned int              j) const
{
  if (j == this->m_HighPassSubBands)
  {
    return this->EvaluateForwardHighPassFilter(freq_norm_in_hz);
  }
  if (j == 0)
  {
    return this->EvaluateForwardLowPassFilter(freq_norm_in_hz);
  }
  if (j > this->m_HighPassSubBands || j < 0)
  {
    itkExceptionMacro(<< "Invalid sub-band.");
  }
  FunctionValueType value = std::pow(freq_norm_in_hz, static_cast<int>(this->m_HighPassSubBands)) *
                            std::pow(2.0, static_cast<int>(2 * this->m_HighPassSubBands - 1 - j));
  return this->EvaluateMagnitude(value);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::FunctionValueType
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateInverseLowPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  return this->EvaluateForwardLowPassFilter(freq_norm_in_hz);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::FunctionValueType
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateInverseHighPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  return this->EvaluateForwardHighPassFilter(freq_norm_in_hz);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::FunctionValueType
IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateInverseSubBand(
  const FunctionValueType & freq_norm_in_hz,
  unsigned int              j) const
{
  return this->EvaluateForwardSubBand(freq_norm_in_hz, j);
}
} // end namespace itk

#endif
