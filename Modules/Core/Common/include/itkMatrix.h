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
#ifndef itkMatrix_h
#define itkMatrix_h

#include "itkPoint.h"
#include "itkCovariantVector.h"

#include <vxl_version.h>
#include "vnl/vnl_matrix_fixed.hxx" // Get the templates
#include "vnl/vnl_transpose.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_determinant.h"
#include "itkMath.h"

namespace itk
{
/**
 *\class Matrix
 * \brief A templated class holding a M x N size Matrix.
 *
 * This class contains a vnl_matrix_fixed in order
 * to make all the vnl mathematical methods available.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/Matrix,Matrix}
 * \sphinxexample{Core/Common/MatrixInverse, Matrix Inverse}
 * \endsphinx
 */

template <typename T, unsigned int NRows = 3, unsigned int NColumns = 3>
class ITK_TEMPLATE_EXPORT Matrix
{
public:
  /** Standard class type aliases. */
  using Self = Matrix;

  /** Component value type */
  using ValueType = T;
  using ComponentType = T;

  /** Number Of Columns and Rows. */
  static constexpr unsigned int RowDimensions = NRows;
  static constexpr unsigned int ColumnDimensions = NColumns;

  /** Internal matrix type */
  using InternalMatrixType = vnl_matrix_fixed<T, NRows, NColumns>;

  /** Compatible square matrix. This is currently used by operator* to help
   * with wrapping.  \todo In the future, the method should be templated to allow
   * multiplication by NColumns by XRows.*/
  using CompatibleSquareMatrixType = Matrix<T, NColumns, NColumns>;

  /** Matrix by Vector multiplication.  */
  Vector<T, NRows> operator*(const Vector<T, NColumns> & vect) const;

  /** Matrix by Point multiplication.  */
  Point<T, NRows> operator*(const Point<T, NColumns> & pnt) const;

  /** Matrix by CovariantVector multiplication.  */
  CovariantVector<T, NRows> operator*(const CovariantVector<T, NColumns> & covect) const;

  /** Matrix by vnl_vector_fixed multiplication.  */
  vnl_vector_fixed<T, NRows> operator*(const vnl_vector_fixed<T, NColumns> & inVNLvect) const;

  /** Matrix by Matrix multiplication.  */
  Self operator*(const CompatibleSquareMatrixType & matrix) const;

  template <unsigned int OuterDim>
  Matrix<T, NRows, OuterDim> operator*(const vnl_matrix_fixed<T, NRows, OuterDim> & matrix) const
  {
    const Matrix<T, NRows, OuterDim> result(m_Matrix * matrix);
    return result;
  }

  /** Matrix addition.  */
  Self
  operator+(const Self & matrix) const;

  const Self &
  operator+=(const Self & matrix);

  /** Matrix addition.  */
  Self
  operator-(const Self & matrix) const;

  const Self &
  operator-=(const Self & matrix);

  /** Matrix by vnl_matrix multiplication.  */
  vnl_matrix<T> operator*(const vnl_matrix<T> & matrix) const;

  /** Matrix by Matrix multiplication.  */
  void
  operator*=(const CompatibleSquareMatrixType & matrix);

  /** Matrix by vnl_matrix multiplication.  */
  void
  operator*=(const vnl_matrix<T> & matrix);

  /** Matrix by vnl_vector multiplication.  */
  vnl_vector<T> operator*(const vnl_vector<T> & vc) const;

  /** Matrix by scalar multiplication. */
  void
  operator*=(const T & value)
  {
    m_Matrix *= value;
  }

  /** Matrix by scalar multiplication.  */
  Self operator*(const T & value) const
  {
    Self result(*this);

    result *= value;
    return result;
  }

  /** Matrix by scalar division. */
  void
  operator/=(const T & value)
  {
    m_Matrix /= value;
  }

  /** Matrix by scalar division. */
  Self
  operator/(const T & value) const
  {
    Self result(*this);

    result /= value;
    return result;
  }

  /** Return an element of the matrix. */
  inline T &
  operator()(unsigned int row, unsigned int col)
  {
    return m_Matrix(row, col);
  }

  /** Return an element of the matrix. */
  inline const T &
  operator()(unsigned int row, unsigned int col) const
  {
    return m_Matrix(row, col);
  }

  /** Return a row of the matrix. */
  inline T * operator[](unsigned int i) { return m_Matrix[i]; }

  /** Return a row of the matrix. */
  inline const T * operator[](unsigned int i) const { return m_Matrix[i]; }

  /** Return the matrix. */
  inline InternalMatrixType &
  GetVnlMatrix()
  {
    return m_Matrix;
  }

  /** Return the matrix. */
  inline const InternalMatrixType &
  GetVnlMatrix() const
  {
    return m_Matrix;
  }

  /** Set the matrix to identity. */
  inline void
  SetIdentity()
  {
    m_Matrix.set_identity();
  }

  /** Get an identity matrix. */
  static Self
  GetIdentity()
  {
    InternalMatrixType internalMatrix;
    internalMatrix.set_identity();
    return Self{ internalMatrix };
  }

  /** Fill the matrix with a value. */
  inline void
  Fill(const T & value)
  {
    m_Matrix.fill(value);
  }

  /** Assignment operator. */
  inline const Self &
  operator=(const vnl_matrix<T> & matrix)
  {
    m_Matrix = matrix;
    return *this;
  }

  /**For every operator=, there should be an equivalent copy constructor. */
  inline Matrix(const vnl_matrix<T> & matrix)
    : m_Matrix(matrix)
  {}

  /** Comparison operators. */
  inline bool
  operator==(const Self & matrix) const
  {
    bool equal = true;

    for (unsigned int r = 0; r < NRows; r++)
    {
      for (unsigned int c = 0; c < NColumns; c++)
      {
        if (Math::NotExactlyEquals(m_Matrix(r, c), matrix.m_Matrix(r, c)))
        {
          equal = false;
          break;
        }
      }
    }
    return equal;
  }

  inline bool
  operator!=(const Self & matrix) const
  {
    return !this->operator==(matrix);
  }

  inline const Self &
  operator=(const InternalMatrixType & matrix)
  {
    this->m_Matrix = matrix;
    return *this;
  }

  /**For every operator=, there should be an equivalent copy constructor. */
  inline explicit Matrix(const InternalMatrixType & matrix)
    : m_Matrix(matrix)
  {}

  /** Return the inverse matrix. */
  inline vnl_matrix_fixed<T, NColumns, NRows>
  GetInverse() const
  {
    if (vnl_determinant(m_Matrix) == NumericTraits<T>::ZeroValue())
    {
      itkGenericExceptionMacro(<< "Singular matrix. Determinant is 0.");
    }
    vnl_matrix_inverse<T> inverse(m_Matrix.as_ref());
    return vnl_matrix_fixed<T, NColumns, NRows>{ inverse.as_matrix() };
  }

  /** Return the transposed matrix. */
  inline vnl_matrix_fixed<T, NColumns, NRows>
  GetTranspose() const
  {
    return vnl_matrix_fixed<T, NColumns, NRows>{ m_Matrix.transpose().as_matrix() };
  }

  /** Defaulted default-constructor. Zero-initializes all of its elements.
   * \note The other five "special member functions" (copy-constructor,
   * copy-assignment operator, move-constructor, move-assignment operator,
   * and destructor) are implicitly defaulted, following the C++ "Rule of Zero".
   */
  Matrix() = default;

  void
  swap(Self & other)
  {
    // allow for augment dependent look up
    using std::swap;
    swap(this->m_Matrix, other.m_Matrix);
  }

private:
  InternalMatrixType m_Matrix{};
};

template <typename T, unsigned int NRows, unsigned int NColumns>
std::ostream &
operator<<(std::ostream & os, const Matrix<T, NRows, NColumns> & v)
{
  os << v.GetVnlMatrix();
  return os;
}


template <typename T, unsigned int NRows, unsigned int NColumns>
inline void
swap(const Matrix<T, NRows, NColumns> & a, const Matrix<T, NRows, NColumns> & b)
{
  a.swap(b);
}

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMatrix.hxx"
#endif

#endif
