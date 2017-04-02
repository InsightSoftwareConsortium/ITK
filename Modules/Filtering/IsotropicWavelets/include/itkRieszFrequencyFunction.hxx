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
#include <algorithm>

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::RieszFrequencyFunction()
  : m_Order(1)
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
  return OutputComplexType(0, static_cast<typename OutputComplexType::value_type>(-frequency_point[direction] / magn));
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::OutputComplexType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateWithIndices(
  const TInput &                frequency_point,
  const IndicesFixedArrayType & indices)
{
  this->SetIndices(indices);
  double magn(this->Magnitude(frequency_point));

  // Precondition:
  if (itk::Math::FloatAlmostEqual(magn, 0.0))
  {
    return OutputComplexType(0);
  }

  // freqProduct = w1^n1...wd^nd
  double freqProduct(1);
  for (unsigned int dim = 0; dim < VImageDimension; ++dim)
  {
    for (unsigned int n = 0; n < indices[dim]; ++n)
    {
      freqProduct *= frequency_point[dim];
    }
  }

  // rieszComponent = (-j)^{m_Order} * sqrt(m_Order!/(n1!n2!...nd!)) * w1^n1...wd^nd / ||w||^m_Order
  return this->m_NormalizingIndicesComplexFactor * static_cast<typename OutputComplexType::value_type>(
                                                     freqProduct / std::pow(magn, static_cast<double>(this->m_Order)));
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
    out[dim] = OutputComplexType(0, static_cast<typename OutputComplexType::value_type>(-frequency_point[dim] / magn));
  }
  return out;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::ComputeUniqueIndices(IndicesArrayType subIndice,
                                                                                      unsigned int     init,
                                                                                      SetType &        uniqueIndices)
{
  unsigned int subIndiceSize = subIndice.size();
  if (init == subIndiceSize - 1)
  {
    return;
  }

  // If OK, store it.
  if (std::distance(subIndice.begin(), std::max_element(subIndice.begin() + init, subIndice.end())) <= init)
  {
    IndicesArrayType subIndiceCopy = subIndice;
    std::sort(subIndiceCopy.rbegin(), subIndiceCopy.rend());
    uniqueIndices.insert(subIndiceCopy);
  }
  else
  {
    // Process remeaning indice positions in this branch.
    Self::ComputeUniqueIndices(subIndice, init + 1, uniqueIndices);
    return;
  }

  unsigned int first = --subIndice[init];
  ++subIndice[init + 1];
  // Stop
  if (first == 0)
  {
    return;
  }

  // Process modified subIndice.
  Self::ComputeUniqueIndices(subIndice, init, uniqueIndices);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::SetType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::ComputeAllPermutations(const SetType & uniqueIndices)
{
  SetType out;
  for (typename SetType::const_iterator it = uniqueIndices.begin(); it != uniqueIndices.end(); ++it)
  {
    out.insert(*it);
    IndicesArrayType permutation = *it;
    while (std::prev_permutation(permutation.begin(), permutation.end()))
    {
      out.insert(permutation);
    }
  }
  return out;
}

} // end namespace itk

#endif
