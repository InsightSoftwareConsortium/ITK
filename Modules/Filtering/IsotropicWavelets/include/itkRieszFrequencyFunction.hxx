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
  : m_Order(0)
{
  // SetOrder also sets m_Indices.
  this->SetOrder(1);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::~RieszFrequencyFunction()
{}

// template< typename TFunctionValue, unsigned int VImageDimension, typename TInput >
// typename RieszFrequencyFunction< TFunctionValue, VImageDimension, TInput >::OutputComplexType
// RieszFrequencyFunction< TFunctionValue, VImageDimension, TInput >
// ::Evaluate(
//   const TInput & frequency_point,
//   const unsigned int & direction) const
// {
//   double magn(this->Magnitude(frequency_point));
//
//   if(itk::Math::FloatAlmostEqual(magn, 0.0) )
//     {
//     return OutputComplexType(0);
//     }
//   return OutputComplexType(0, static_cast<typename OutputComplexType::value_type>( -frequency_point[direction] / magn
//   ) );
// }

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::OutputComplexType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateWithIndices(const TInput & frequency_point,
                                                                                     const IndicesArrayType & indices)
{
  double magn(this->Magnitude(frequency_point));

  // Precondition:
  // TODO: default precision ok? :
  // https://itk.org/Doxygen/html/namespaceitk_1_1Math.html#ae9f0d6137957033eecb66c0e1356d022
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
  return this->ComputeNormalizingFactor(indices) * static_cast<typename OutputComplexType::value_type>(
                                                     freqProduct / std::pow(magn, static_cast<double>(this->m_Order)));
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
unsigned int
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::ComputeNumberOfComponents(unsigned int order)
{
  return Self::Factorial(order + VImageDimension - 1) / (Self::Factorial(VImageDimension - 1) * Self::Factorial(order));
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::ComputeUniqueIndices(IndicesArrayType subIndice,
                                                                                      SetType &        uniqueIndices,
                                                                                      unsigned int     init)
{
  unsigned int subIndiceSize = subIndice.size();
  if (init == subIndiceSize - 1)
  {
    return;
  }

  // If OK, store it.
  if (std::distance(subIndice.begin(),
                    std::max_element(subIndice.begin(), subIndice.end(), std::greater<unsigned int>())) <=
      VImageDimension - 1)
  {
    IndicesArrayType subIndiceCopy = subIndice;
    std::sort(subIndiceCopy.rbegin(), subIndiceCopy.rend());
    uniqueIndices.insert(subIndiceCopy);
  }
  else
  {
    // Process remaining index positions in this branch.
    Self::ComputeUniqueIndices(subIndice, uniqueIndices, init + 1);
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
  Self::ComputeUniqueIndices(subIndice, uniqueIndices, init);
  // Process modified init.
  Self::ComputeUniqueIndices(subIndice, uniqueIndices, init + 1);
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
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::OutputComplexType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::ComputeNormalizingFactor(
  const IndicesArrayType & indices) const
{
  // normalizeFactor: sqrt(m_Order!/(n1!n2!...nd!))
  typename OutputComplexType::value_type normalizeFactor(1);
  for (unsigned int dim = 0; dim < VImageDimension; ++dim)
  {
    normalizeFactor *= Self::Factorial(indices[dim]);
  }
  normalizeFactor = sqrt(Self::Factorial(this->m_Order) / normalizeFactor);

  // assert(this->m_Order > 0)
  // calculate (-j)^m_Order
  // dev: hacky switch because precision in std::pow<complex> is a source of important numerical errors.
  OutputComplexType powComplex(0, 0);
  unsigned int      modulo4 = this->m_Order % 4;
  switch (modulo4)
  {
    case 1:
      powComplex.imag(-1);
      break;
    case 2:
      powComplex.real(1);
      break;
    case 3:
      powComplex.imag(1);
      break;
    case 0:
      powComplex.real(-1);
      break;
  }
  // itkDebugMacro(<< "\n (-j)^Order: "<< powComplex << " output: " << normalizeFactor * powComplex)

  // (-j)^{m_Order} * sqrt(m_Order!/(n1!n2!...nd!))
  return powComplex * normalizeFactor;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
typename RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::OutputComponentsType
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::EvaluateAllComponents(
  const TInput & frequency_point) const
{
  double magn(this->Magnitude(frequency_point));
  // Precondition:
  if (itk::Math::FloatAlmostEqual(magn, 0.0))
  {
    // return empty vector of length equal to NumberOfComponents.
    return OutputComponentsType(RieszFrequencyFunction::ComputeNumberOfComponents(this->m_Order));
  }

  const SetType & allIndices = this->m_Indices;

  OutputComponentsType out;
  for (typename SetType::const_iterator it = allIndices.begin(); it != allIndices.end(); ++it)
  {
    IndicesArrayType indice = *it;
    // freqProduct = w1^n1...wd^nd
    double freqProduct(1);
    for (unsigned int dim = 0; dim < VImageDimension; ++dim)
    {
      for (unsigned int n = 0; n < indice[dim]; ++n)
      {
        freqProduct *= frequency_point[dim];
      }
    }
    // rieszComponent = (-j)^{m_Order} * sqrt(m_Order!/(n1!n2!...nd!)) * w1^n1...wd^nd / ||w||^m_Order
    OutputComplexType outPerIndice = this->ComputeNormalizingFactor(indice);
    outPerIndice *= static_cast<typename OutputComplexType::value_type>(
      freqProduct / std::pow(magn, static_cast<double>(this->m_Order)));
    out.push_back(outPerIndice);
  }

  return out;
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
void
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Order: " << this->m_Order << std::endl;
  os << indent << "m_Indices:" << std::endl;
  for (typename SetType::const_iterator it = this->m_Indices.begin(); it != this->m_Indices.end(); ++it)
  {
    std::cout << "(";
    for (unsigned int i = 0; i < VImageDimension; ++i)
    {
      std::cout << (*it)[i] << ", ";
    }
    std::cout << ")" << std::endl;
  }
}

} // end namespace itk

#endif
