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
#ifndef itkVowIsotropicWavelet_hxx
#define itkVowIsotropicWavelet_hxx

#include <cmath>
#include <itkMath.h>
#include <itkVowIsotropicWavelet.h>

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::VowIsotropicWavelet()
  // Value mentioned in original article. Not recommended to modify.
  : m_Kappa(0.75)
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::~VowIsotropicWavelet()
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateMagnitude(
  const FunctionValueType & freq_norm_in_hz) const
{
  // freq_in_rad_per_sec = freq_norm_in_hz * 2 * pi
  if (freq_norm_in_hz >= 1 / 8.0 && freq_norm_in_hz < 1 / 4.0)
    return static_cast<TFunctionValue>(
      sqrt(0.5 + std::tan(this->m_Kappa * (1 + 2 * std::log2(4 * freq_norm_in_hz))) / 2 * std::tan(this->m_Kappa)));

  if (freq_norm_in_hz >= 1 / 4.0 && freq_norm_in_hz <= 1 / 2.0)
    return static_cast<TFunctionValue>(
      sqrt(0.5 - std::tan(this->m_Kappa * (1 + 2 * std::log2(2 * freq_norm_in_hz))) / 2 * std::tan(this->m_Kappa)));
  return 0;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateForwardLowPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  FunctionValueType value =
    std::pow(freq_norm_in_hz, this->m_HighPassSubBands) * std::pow(2.0, 2 * this->m_HighPassSubBands - 1);
  if (value > 0.25)
    return this->EvaluateMagnitude(value);
  return 1;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateForwardHighPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  FunctionValueType value =
    std::pow(freq_norm_in_hz, this->m_HighPassSubBands) * std::pow(2.0, this->m_HighPassSubBands - 1);
  if (value < 0.25)
    return this->EvaluateMagnitude(value);
  return 1;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateForwardSubBand(
  const FunctionValueType & freq_norm_in_hz,
  unsigned int              j) const
{
  if (j == this->m_HighPassSubBands)
    return this->EvaluateForwardHighPassFilter(freq_norm_in_hz);
  if (j == 0)
    return this->EvaluateForwardLowPassFilter(freq_norm_in_hz);
  if (j > this->m_HighPassSubBands || j < 0)
    throw itk::ExceptionObject(__FILE__, __LINE__, "Invalid SubBand", ITK_LOCATION);
  FunctionValueType value =
    std::pow(freq_norm_in_hz, this->m_HighPassSubBands) * std::pow(2.0, 2 * this->m_HighPassSubBands - 1 - j);
  return this->EvaluateMagnitude(value);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateInverseLowPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  return this->EvaluateForwardLowPassFilter(freq_norm_in_hz);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateInverseHighPassFilter(
  const FunctionValueType & freq_norm_in_hz) const
{
  return this->EvaluateForwardHighPassFilter(freq_norm_in_hz);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateInverseSubBand(
  const FunctionValueType & freq_norm_in_hz,
  unsigned int              j) const
{
  return this->EvaluateForwardSubBand(freq_norm_in_hz, j);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kappa: " << this->m_Kappa << std::endl;
}
} // end namespace itk

#endif
