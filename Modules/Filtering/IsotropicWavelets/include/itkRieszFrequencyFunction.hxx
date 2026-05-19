/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include <algorithm>

namespace itk
{
template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::RieszFrequencyFunction()

{
  // SetOrder also sets m_Indices.
  this->SetOrder(1);
}

template <typename TFunctionValue, unsigned int VImageDimension, typename TInput>
RieszFrequencyFunction<TFunctionValue, VImageDimension, TInput>::~RieszFrequencyFunction() = default;

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
    case 1: //(-j)
      powComplex = OutputComplexType(0, -1);
      break;
    case 2: // (-j)(-j) = -1
      powComplex = OutputComplexType(-1, 0);
      break;
    case 3: // -1(-j) = j
      powComplex = OutputComplexType(0, 1);
      break;
    case 0: // j(-j) = 1
      powComplex = OutputComplexType(1, 0);
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
  for (auto index : allIndices)
  {
    // freqProduct = w1^n1...wd^nd
    double freqProduct(1);
    for (unsigned int dim = 0; dim < VImageDimension; ++dim)
    {
      if (index[dim] > 0)
      {
        freqProduct *= std::pow(static_cast<double>(frequency_point[dim]), static_cast<double>(index[dim]));
      }
      // for (unsigned int n = 0; n<index[dim]; ++n)
      //   {
      //   freqProduct *= frequency_point[dim];
      //   }
    }
    // rieszComponent = (-j)^{m_Order} * sqrt(m_Order!/(n1!n2!...nd!)) * w1^n1...wd^nd / ||w||^m_Order
    OutputComplexType outPerIndex = this->ComputeNormalizingFactor(index);
    outPerIndex *= static_cast<typename OutputComplexType::value_type>(
      freqProduct / std::pow(magn, static_cast<double>(this->m_Order)));
    out.push_back(outPerIndex);
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
  for (auto it = this->m_Indices.begin(); it != this->m_Indices.end(); ++it)
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
