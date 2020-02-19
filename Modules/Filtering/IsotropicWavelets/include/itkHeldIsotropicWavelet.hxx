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
#ifndef itkHeldIsotropicWavelet_hxx
#define itkHeldIsotropicWavelet_hxx

#include <cmath>
#include <itkMath.h>
#include <itkHeldIsotropicWavelet.h>

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
HeldIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::HeldIsotropicWavelet()

  = default;

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
HeldIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::~HeldIsotropicWavelet() = default;

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
HeldIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PolynomialOrder: " << this->m_PolynomialOrder << std::endl;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename HeldIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
HeldIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateMagnitude(
  const FunctionValueType & freq_norm_in_hz) const
{
  // freq_in_rad_per_sec = freq_norm_in_hz * 2 * pi
  if (freq_norm_in_hz > 0.125 && freq_norm_in_hz <= 0.25)
  {
    return static_cast<TFunctionValue>(
      std::cos(2.0 * Math::pi * this->ComputePolynom(freq_norm_in_hz, this->m_PolynomialOrder)));
  }

  if (freq_norm_in_hz > 0.25 && freq_norm_in_hz <= 0.5)
  {
    return static_cast<TFunctionValue>(
      std::sin(2.0 * Math::pi * this->ComputePolynom(freq_norm_in_hz / 2.0, this->m_PolynomialOrder)));
  }

  return 0;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename HeldIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
HeldIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::ComputePolynom(const FunctionValueType & x,
                                                                              const unsigned int &      order) const
{
  double y = 0.0;
  double x8 = 8 * static_cast<double>(x);

  switch (order)
  {
    case 0:
      y = 0.5 - 0.25 * x8;
      break;
    case 1:
      y = -1.0 + 3.0 * x8 - 2.25 * std::pow(x8, 2.0) + 0.5 * std::pow(x8, 3.0);
      break;
    case 2:
      y = 8.0 - 30.0 * x8 + 45.0 * std::pow(x8, 2.0) - 32.5 * std::pow(x8, 3.0) + 11.25 * std::pow(x8, 4.0) -
          1.5 * std::pow(x8, 5.0);
      break;
    case 3:
      y = -52.0 + 280.0 * x8 - 630.0 * std::pow(x8, 2.0) + 770.0 * std::pow(x8, 3.0) - 551.25 * std::pow(x8, 4.0) +
          231.0 * std::pow(x8, 5.0) - 52.5 * std::pow(x8, 6.0) + 5.0 * std::pow(x8, 7.0);
      break;
    case 4:
      y = 368.0 - 2520.0 * x8 + 7560.0 * std::pow(x8, 2.0) - 13020.0 * std::pow(x8, 3.0) + 14175.0 * std::pow(x8, 4.0) -
          10111.5 * std::pow(x8, 5.0) + 4725.0 * std::pow(x8, 6.0) - 1395.0 * std::pow(x8, 7.0) +
          236.25 * std::pow(x8, 8.0) - 17.5 * std::pow(x8, 9.0);
      break;
    case 5:
      y = -2656.0 + 22176.0 * x8 - 83160.0 * std::pow(x8, 2.0) + 184800.0 * std::pow(x8, 3.0) -
          270270.0 * std::pow(x8, 4.0) + 273042.0 * std::pow(x8, 5.0) - 194386.5 * std::pow(x8, 6.0) +
          97515.0 * std::pow(x8, 7.0) - 33783.75 * std::pow(x8, 8.0) + 7700.0 * std::pow(x8, 9.0) -
          1039.5 * std::pow(x8, 10.0) + 63.0 * std::pow(x8, 11.0);
      break;
    default:
      itkExceptionMacro(<< "Order of polynom must be less than 6.");
  }
  return static_cast<FunctionValueType>(y);
}
} // end namespace itk

#endif
