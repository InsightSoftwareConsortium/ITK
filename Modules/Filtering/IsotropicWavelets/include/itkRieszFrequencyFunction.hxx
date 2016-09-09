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
#ifndef itkRieszFrequencyFunction_hxx
#define itkRieszFrequencyFunction_hxx

#include <cmath>
#include <itkMath.h>
#include "itkRieszFrequencyFunction.h"

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::RieszFrequencyFunction()
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::~RieszFrequencyFunction()
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::FunctionValueType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::Evaluate(const TInput &       frequency_point,
                                                                          const unsigned int & dimension) const
{
  double magn(this->Magnitude(frequency_point));
  if (itk::Math::FloatAlmostEqual(magn, 0.0))
    return FunctionValueType(0);
  return static_cast<FunctionValueType>(frequency_point[dimension] / magn);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::InputType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateArray(const TInput & frequency_point) const
{
  double magn(this->Magnitude(frequency_point));
  if (itk::Math::FloatAlmostEqual(magn, 0.0))
    return InputType(0);
  InputType out;
  for (unsigned int dim = 0; dim < VImageDimension; ++dim)
  {
    out[dim] = frequency_point[dim] / magn;
  }
  return out;
}
} // end namespace itk

#endif
