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
  : m_Order(1)
  , m_OrderFactorial(1)
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::~RieszFrequencyFunction()
{}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Order: " << this->m_Order << std::endl;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::OutputComplexType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::Evaluate(const TInput &       frequency_point,
                                                                          const unsigned int & direction) const
{
  double magn(this->Magnitude(frequency_point));

  if (itk::Math::FloatAlmostEqual(magn, 0.0))
  {
    return OutputComplexType(0);
  }
  return OutputComplexType(0, static_cast<FunctionValueType>(-frequency_point[direction] / magn));
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::OutputComplexType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateWithIndices(
  const TInput &           frequency_point,
  const IndicesArrayType & indices) const
{
  double magn(this->Magnitude(frequency_point));

  if (itk::Math::FloatAlmostEqual(magn, 0.0))
  {
    return OutputComplexType(0);
  }

  // freqProduct = Product (freq[dim]^indices[dim])
  typename OutputComplexType::value_type freqProduct(1);
  for (unsigned int dim = 0; dim < VImageDimension; ++dim)
  {
    for (unsigned int n = 0; n < indices[dim]; ++n)
    {
      freqProduct *= frequency_point[dim];
    }
  }
  // normalizeFactor:
  typename OutputComplexType::value_type normalizeFactor(1);
  for (unsigned int dim = 0; dim < VImageDimension; ++dim)
  {
    normalizeFactor *= Self::Factorial(indices[dim]);
  }
  normalizeFactor = sqrt(this->m_OrderFactorial / normalizeFactor);

  // calculate (-j)^m_Order
  OutputComplexType out(0, -1); // -j
  out = std::pow(out, static_cast<typename OutputComplexType::value_type>(this->m_Order));
  return out *= normalizeFactor * freqProduct / std::pow(magn, static_cast<double>(this->m_Order));
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::OutputComplexArrayType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateArray(const TInput & frequency_point) const
{
  double magn(this->Magnitude(frequency_point));

  // TODO: default precision ok? :
  // https://itk.org/Doxygen/html/namespaceitk_1_1Math.html#ae9f0d6137957033eecb66c0e1356d022
  if (itk::Math::FloatAlmostEqual(magn, 0.0))
  {
    return OutputComplexArrayType(0);
  }
  OutputComplexArrayType out;
  for (unsigned int dim = 0; dim < VImageDimension; ++dim)
  {
    out[dim] = OutputComplexType(0, static_cast<FunctionValueType>(-frequency_point[dim] / magn));
  }
  return out;
}
} // end namespace itk

#endif
