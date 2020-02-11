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
#ifndef itkNumericTraitsArrayPixel_h
#define itkNumericTraitsArrayPixel_h

#include "itkNumericTraits.h"
#include "itkArray.h"

namespace itk
{
/** \brief NumericTraits for Array
 * \tparam T type of the array component
 */
template <typename T>
class NumericTraits<Array<T>>
{
private:
  using ElementAbsType = typename NumericTraits<T>::AbsType;
  using ElementAccumulateType = typename NumericTraits<T>::AccumulateType;
  using ElementFloatType = typename NumericTraits<T>::FloatType;
  using ElementPrintType = typename NumericTraits<T>::PrintType;
  using ElementRealType = typename NumericTraits<T>::RealType;

public:
  /** Return the type of the native component type. */
  using ValueType = T;
  using Self = Array<T>;

  /** Unsigned component type */
  using AbsType = Array<ElementAbsType>;

  /** Accumulation of addition and multiplication. */
  using AccumulateType = Array<ElementAccumulateType>;

  /** Typedef for operations that use floating point instead of real precision
   */
  using FloatType = Array<ElementFloatType>;

  /** Return the type that can be printed. */
  using PrintType = Array<ElementPrintType>;

  /** Type for real-valued scalar operations. */
  using RealType = Array<ElementRealType>;

  /** Type for real-valued scalar operations. */
  using ScalarRealType = ElementRealType;

  /** Measurement vector type */
  using MeasurementVectorType = Self;

  /** Component wise defined element
   *
   * \note minimum value for floating pointer types is defined as
   * minimum positive normalize value.
   */
  static const Self
  max(const Self & a)
  {
    Self b(a.Size());

    b.Fill(NumericTraits<T>::max());
    return b;
  }

  static const Self
  min(const Self & a)
  {
    Self b(a.Size());

    b.Fill(NumericTraits<T>::min());
    return b;
  }

  static const Self
  ZeroValue(const Self & a)
  {
    Self b(a.Size());

    b.Fill(NumericTraits<T>::ZeroValue());
    return b;
  }

  static const Self
  OneValue(const Self & a)
  {
    Self b(a.Size());

    b.Fill(NumericTraits<T>::OneValue());
    return b;
  }

  static const Self
  NonpositiveMin(const Self & a)
  {
    Self b(a.Size());
    b.Fill(NumericTraits<T>::NonpositiveMin());
    return b;
  }

  static constexpr bool IsSigned = NumericTraits<ValueType>::IsSigned;
  static constexpr bool IsInteger = NumericTraits<ValueType>::IsInteger;
  static constexpr bool IsComplex = NumericTraits<ValueType>::IsComplex;

  /** Set the length of the input array and fill it with zeros. */
  static void
  SetLength(Array<T> & m, const unsigned int s)
  {
    m.SetSize(s);
    m.Fill(NumericTraits<T>::ZeroValue());
  }

  /** Get the length of the input array. */
  static std::size_t
  GetLength(const Array<T> & m)
  {
    return m.GetSize();
  }

  static void
  AssignToArray(const Self & v, MeasurementVectorType & mv)
  {
    mv = v;
  }

  template <typename TArray>
  static void
  AssignToArray(const Self & v, TArray & mv)
  {
    for (unsigned int i = 0; i < GetLength(v); i++)
    {
      mv[i] = v[i];
    }
  }
};
} // end namespace itk

#endif // itkNumericTraitsArrayPixel_h
