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

#ifndef itkNumericTraitsStdVector_h
#define itkNumericTraitsStdVector_h

#include "itkMath.h"
#include <vector>

// This work is part of the National Alliance for Medical Image Computing
// (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
// for Medical Research, Grant U54 EB005149.

namespace itk
{
/**\class NumericTraits
 * \brief Define numeric traits for std::vector.
 * \tparam T Component type of std::vector
 *
 * We provide here a generic implementation based on creating types of
 * std::vector whose components are the types of the NumericTraits from
 * the original std::vector components. This implementation require
 * support for partial specializations, since it is based on the
 * concept that:
 *   NumericTraits<std::vector< T > >  is defined piecewise by
 *   std::vector< NumericTraits< T > >
 *
 * \note The Zero(), One(), min() and max() methods here take
 * references to a pixel as input.  This is due to the fact that the
 * length of the std::vector is not known until
 * run-time. Since the most common use of Zero and One is for
 * comparison purposes or initialization of sums etc, this might just
 * as easily be re-written with a pixel passed in as a reference and
 * the length is inferred from this pixel.
 *
 * \sa NumericTraits
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <typename T>
class NumericTraits<std::vector<T>>
{
public:
  using ElementAbsType = typename NumericTraits<T>::AbsType;
  using ElementAccumulateType = typename NumericTraits<T>::AccumulateType;
  using ElementFloatType = typename NumericTraits<T>::FloatType;
  using ElementPrintType = typename NumericTraits<T>::PrintType;
  using ElementRealType = typename NumericTraits<T>::RealType;

  /** Return the type of the native component type. */
  using ValueType = T;

  using Self = std::vector<T>;

  /** Unsigned component type */
  using AbsType = std::vector<ElementAbsType>;

  /** Accumulation of addition and multiplication. */
  using AccumulateType = std::vector<ElementAccumulateType>;

  /** Typedef for operations that use floating point instead of real precision
   */
  using FloatType = std::vector<ElementFloatType>;

  // TODO: this won't really print well, at least not without defining an operator
  // to push to a stream.
  /** Return the type that can be printed. */
  using PrintType = std::vector<ElementPrintType>;

  /** Type for real-valued scalar operations. */
  using RealType = std::vector<ElementRealType>;

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
    Self b(a.Size(), NumericTraits<T>::max());
    return b;
  }

  static const Self
  min(const Self & a)
  {
    Self b(a.Size(), NumericTraits<T>::min());
    return b;
  }

  static const Self
  ZeroValue(const Self & a)
  {
    Self b(a.Size(), NumericTraits<T>::ZeroValue());
    return b;
  }

  static const Self
  OneValue(const Self & a)
  {
    Self b(a.Size(), NumericTraits<T>::OneValue());
    return b;
  }

  static const Self
  NonpositiveMin(const Self & a)
  {
    Self b(a.Size(), NumericTraits<T>::NonpositiveMin());
    return b;
  }

  static constexpr bool IsSigned = NumericTraits<ValueType>::IsSigned;
  static constexpr bool IsInteger = NumericTraits<ValueType>::IsInteger;
  static constexpr bool IsComplex = NumericTraits<ValueType>::IsComplex;

  /** Resize the input vector to the specified size */
  static void
  SetLength(std::vector<T> & m, const unsigned int s)
  {
    // since std::vector often holds types that have no NumericTraits::ZeroValue(),
    // allow resize() to call the type's default constructor
    m.clear();
    m.resize(s);
  }

  /** Return the size of the vector. */
  static unsigned int
  GetLength(const std::vector<T> & m)
  {
    return itk::Math::CastWithRangeCheck<unsigned int>(m.size());
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

#endif // itkNumericTraitsStdVector_h
