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
#ifndef itkShannonIsotropicWavelet_hxx
#define itkShannonIsotropicWavelet_hxx

#include <cmath>
#include <itkMath.h>
#include <itkShannonIsotropicWavelet.h>

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
ShannonIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::ShannonIsotropicWavelet()
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
ShannonIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::~ShannonIsotropicWavelet()
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
ShannonIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename ShannonIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::FunctionValueType
ShannonIsotropicWavelet<TFunctionValue, VImageDimension, TInput>::EvaluateMagnitude(
  const FunctionValueType & freq_norm_in_hz) const
{
  // freq_in_rad_per_sec = freq_norm_in_hz * 2 * pi
  if (freq_norm_in_hz >= 0.25 && freq_norm_in_hz <= 0.5)
  {
    return 1;
  }
  return 0;
}
} // end namespace itk

#endif
