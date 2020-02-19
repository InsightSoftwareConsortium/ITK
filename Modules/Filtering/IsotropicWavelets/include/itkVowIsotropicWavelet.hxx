/*=========================================================================
 *
 *  Copyright NumFOCUS
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
  : m_Kappa(0.75)
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::~VowIsotropicWavelet() = default;

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kappa: " << this->m_Kappa << std::endl;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
VowIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateMagnitude(
  const FunctionValueType & freq_norm_in_hz) const
{
  // freq_in_rad_per_sec = freq_norm_in_hz * 2 * pi
  // Dev: std::log2 is c++11 only.  std::log2(x) = std::log(x)/itk::Math::ln2
  if (freq_norm_in_hz >= 1 / 8.0 && freq_norm_in_hz < 1 / 4.0)
  {
    return static_cast<TFunctionValue>(
      sqrt(0.5 + std::tan(this->m_Kappa * (1.0 + (2.0 / itk::Math::ln2) * std::log(4 * freq_norm_in_hz))) /
                   (2.0 * std::tan(this->m_Kappa))));
  }

  if (freq_norm_in_hz >= 1 / 4.0 && freq_norm_in_hz <= 0.5)
  {
    return static_cast<TFunctionValue>(
      sqrt(0.5 - std::tan(this->m_Kappa * (1.0 + (2.0 / itk::Math::ln2) * std::log(2 * freq_norm_in_hz))) /
                   (2.0 * std::tan(this->m_Kappa))));
  }
  return 0;
}
} // end namespace itk

#endif
