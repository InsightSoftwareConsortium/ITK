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
#ifndef itkNumericTraitsTensorPixel_h
#define itkNumericTraitsTensorPixel_h

#include "itkNumericTraits.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{
/**
 * \brief Define numeric traits for SymmetricSecondRankTensor.
 * \tparam T Component type of SymmetricSecondRankTensor
 * \tparam D Dimension of the SymmetricSecondRankTensor
 *
 * We provide here a generic implementation based on creating types of
 * SymmetricSecondRankTensor whose components are the types of the
 * NumericTraits from  the original SymmetricSecondRankTensor
 * components. This implementation require  support for partial
 * specializations, since it is based on the concept that:
 *   NumericTraits<SymmetricSecondRankTensor< T, D > >  is defined piecewise by
 *   SymmetricSecondRankTensor< NumericTraits< T, D > >
 *
 * \sa NumericTraits
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <typename T, unsigned int D>
class NumericTraits<SymmetricSecondRankTensor<T, D>>
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

  using Self = SymmetricSecondRankTensor<T, D>;

  /** Unsigned component type */
  using AbsType = SymmetricSecondRankTensor<ElementAbsType, D>;

  /** Accumulation of addition and multiplication. */
  using AccumulateType = SymmetricSecondRankTensor<ElementAccumulateType, D>;

  /** Typedef for operations that use floating point instead of real precision
   */
  using FloatType = SymmetricSecondRankTensor<ElementFloatType, D>;

  /** Return the type that can be printed. */
  using PrintType = SymmetricSecondRankTensor<ElementPrintType, D>;

  /** Type for real-valued scalar operations. */
  using RealType = SymmetricSecondRankTensor<ElementRealType, D>;

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
  max(const Self &)
  {
    return Self(NumericTraits<T>::max());
  }

  static const Self
  min(const Self &)
  {
    return Self(NumericTraits<T>::min());
  }

  static const Self
  max()
  {
    return Self(NumericTraits<T>::max());
  }

  static const Self
  min()
  {
    return Self(NumericTraits<T>::min());
  }

  static const Self
  NonpositiveMin()
  {
    return Self(NumericTraits<T>::NonpositiveMin());
  }

  static const Self
  ZeroValue()
  {
    return Self(NumericTraits<T>::ZeroValue());
  }

  static const Self
  OneValue()
  {
    return Self(NumericTraits<T>::OneValue());
  }

  static const Self
  NonpositiveMin(const Self &)
  {
    return NonpositiveMin();
  }

  static const Self
  ZeroValue(const Self &)
  {
    return ZeroValue();
  }

  static const Self
  OneValue(const Self &)
  {
    return OneValue();
  }

  static constexpr bool IsSigned = NumericTraits<ValueType>::IsSigned;
  static constexpr bool IsInteger = NumericTraits<ValueType>::IsInteger;
  static constexpr bool IsComplex = NumericTraits<ValueType>::IsComplex;

  /** Fixed length vectors cannot be resized, so an exception will
   *  be thrown if the input size is not valid.  Here, the size refers
   *  to the dimensionality of the underlying FixedArray, not the
   *  tensor dimensionality. */
  static void
  SetLength(SymmetricSecondRankTensor<T, D> & m, const unsigned int s)
  {
    if (s != D * (D + 1) / 2)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a SymmetricSecondRankTensor "
                                  "of dimension "
                               << D << " ( = size of " << D * (D + 1) / 2 << ") to " << s);
    }
    m.Fill(NumericTraits<T>::ZeroValue());
  }

  /** Return the size of the underlying FixedArray. */
  static unsigned int
  GetLength(const SymmetricSecondRankTensor<T, D> &)
  {
    return GetLength();
  }

  /** Return the size of the underlying FixedArray. */
  static unsigned int
  GetLength()
  {
    return D * (D + 1) / 2;
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
    for (unsigned int i = 0; i < GetLength(); i++)
    {
      mv[i] = v[i];
    }
  }

  /** \note: the functions are preferred over the member variables as
   * they are defined for all partial specialization
   */
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};
} // end namespace itk

#endif // itkNumericTraitsTensorPixel_h
