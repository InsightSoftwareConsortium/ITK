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
#ifndef itkNumericTraitsMatrixPixel_h
#define itkNumericTraitsMatrixPixel_h

#include "itkNumericTraits.h"
#include "itkMatrix.h"

namespace itk
{
/** \brief NumericTraits for Matrix
 * \tparam T Component type for Matrix
 * \tparam Nrows Number of rows for the Matrix
 * \tparam Ncols Number of columns for the Matrix
 */
template <typename T, unsigned int Nrows, unsigned int Ncols>
class NumericTraits<Matrix<T, Nrows, Ncols>>
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
  using Self = Matrix<T, Nrows, Ncols>;

  /** Unsigned component type */
  using AbsType = Matrix<ElementAbsType, Nrows, Ncols>;

  /** Accumulation of addition and multiplication. */
  using AccumulateType = Matrix<ElementAccumulateType, Nrows, Ncols>;

  /** Typedef for operations that use floating point instead of real precision
   */
  using FloatType = Matrix<ElementFloatType, Nrows, Ncols>;

  /** Return the type that can be printed. */
  using PrintType = Matrix<ElementPrintType, Nrows, Ncols>;

  /** Type for real-valued scalar operations. */
  using RealType = Matrix<ElementRealType, Nrows, Ncols>;

  /** Type for real-valued scalar operations. */
  using ScalarRealType = ElementRealType;

  /** Measurement Matrix type */
  using MeasurementMatrixType = Self;

  static constexpr unsigned int NumRows = Nrows;
  static constexpr unsigned int NumColumns = Ncols;

  /** Component wise defined element
   *
   * \note minimum value for floating pointer types is defined as
   * minimum positive normalize value.
   */
  static const Self
  max(const Self &)
  {
    Self m;
    m.Fill(NumericTraits<T>::max());
    return m;
  }

  static const Self
  min(const Self &)
  {
    Self m;
    m.Fill(NumericTraits<T>::min());
    return m;
  }

  static const Self
  max()
  {
    Self m;
    m.Fill(NumericTraits<T>::max());
    return m;
  }

  static const Self
  min()
  {
    Self m;
    m.Fill(NumericTraits<T>::min());
    return m;
  }

  static const Self
  NonpositiveMin()
  {
    Self m;
    m.Fill(NumericTraits<T>::NonpositiveMin());
    return m;
  }

  static const Self
  ZeroValue()
  {
    Self m;
    m.Fill(NumericTraits<T>::ZeroValue());
    return m;
  }

  static const Self
  OneValue()
  {
    Self m;
    m.Fill(NumericTraits<T>::OneValue());
    return m;
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

  static bool
  IsPositive(const Self & a)
  {
    bool flag = false;
    for (unsigned int i = 0; i < a.NumRows; ++i)
    {
      for (unsigned int j = 0; j < a.NumColumns; ++j)
      {
        if (a[i][j] > NumericTraits<ValueType>::ZeroValue())
        {
          flag = true;
        }
      }
    }
    return flag;
  }

  static bool
  IsNonpositive(const Self & a)
  {
    bool flag = false;
    for (unsigned int i = 0; i < a.NumRows; ++i)
    {
      for (unsigned int j = 0; j < a.NumColumns; ++j)
      {
        if (!(a[i][j] > 0.0))
        {
          flag = true;
        }
      }
    }
    return flag;
  }

  static bool
  IsNegative(const Self & a)
  {
    bool flag = false;
    for (unsigned int i = 0; i < a.NumRows; ++i)
    {
      for (unsigned int j = 0; j < a.NumColumns; ++j)
      {
        if (a[i][j] < 0.0)
        {
          flag = true;
        }
      }
    }
    return flag;
  }

  static bool
  IsNonnegative(const Self & a)
  {
    bool flag = false;
    for (unsigned int i = 0; i < a.NumRows; ++i)
    {
      for (unsigned int j = 0; j < a.NumColumns; ++j)
      {
        if (!(a[i][j] < 0.0))
        {
          flag = true;
        }
      }
    }
    return flag;
  }

  static constexpr bool IsSigned = NumericTraits<ValueType>::IsSigned;
  static constexpr bool IsInteger = NumericTraits<ValueType>::IsInteger;
  static constexpr bool IsComplex = NumericTraits<ValueType>::IsComplex;

  /** Fixed length Matrixs cannot be resized, so an exception will
   *  be thrown if the input size is not valid.  If the size is valid
   *  the Matrix will be filled with zeros. */
  static void
  SetLength(Matrix<T, Ncols, Nrows> & m, const unsigned int s)
  {
    if (s != Ncols * Nrows)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a Matrix of type " << Ncols << "x" << Nrows << " to " << s);
    }
    m.Fill(NumericTraits<T>::ZeroValue());
  }

  /** Return the size of the Matrix. */
  static unsigned int
  GetLength(const Matrix<T, Nrows, Ncols> &)
  {
    return Nrows * Ncols;
  }

  /** Return the size of the Matrix. */
  static unsigned int
  GetLength()
  {
    return Nrows * Ncols;
  }

  static void
  AssignToArray(const Self & v, MeasurementMatrixType & mv)
  {
    mv = v;
  }

  template <typename TArray>
  static void
  AssignToArray(const Self & v, TArray & mv)
  {
    for (unsigned int i = 0, ij = 0; i < Nrows; ++i)
    {
      for (unsigned int j = 0; j < Ncols; ++j, ++ij)
      {
        mv[ij] = v[i][j];
      }
    }
  }

  /** \note: the functions are preferred over the member variables as
   * they are defined for all partial specialization
   */
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};

// a macro to define and initialize static member variables,
// NOTE: (T)(NumericTraits< T >::[Zero|One]) is needed to generate
//       a temporary variable that is initialized from the
//       constexpr [Zero|One] to be passed by const reference
//       to the GENERIC_MATRIX<T,Nrows,Ncols> constructor.
#define itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, Nrows, Ncols)                                      \
  template <>                                                                                                          \
  ITKCommon_EXPORT const GENERIC_MATRIX<T, Nrows, Ncols> NumericTraits<GENERIC_MATRIX<T, Nrows, Ncols>>::Zero =        \
    GENERIC_MATRIX<T, Nrows, Ncols>((T)(NumericTraits<T>::Zero));                                                      \
  template <>                                                                                                          \
  ITKCommon_EXPORT const GENERIC_MATRIX<T, D> NumericTraits<GENERIC_MATRIX<T, D>>::One =                               \
    GENERIC_MATRIX<T, D>((T)(NumericTraits<T>::One));

//
// List here the matrix dimension specializations of these static
// Traits:
//
#define itkStaticNumericTraitsGenericMatrixDimensionsMacro(GENERIC_MATRIX, T)                                          \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 1, 1);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 1, 2);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 1, 3);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 1, 4);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 2, 1);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 2, 2);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 2, 3);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 2, 4);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 3, 1);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 3, 2);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 3, 3);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 3, 4);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 4, 1);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 4, 2);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 4, 3);                                                   \
  itkStaticNumericTraitsGenericMatrixMacro(GENERIC_MATRIX, T, 4, 4);
} // end namespace itk

#endif // itkNumericTraitsMatrixPixel_h
