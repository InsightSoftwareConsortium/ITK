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
#ifndef itkVariableSizeMatrix_h
#define itkVariableSizeMatrix_h

#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/vnl_transpose.h"
#include "vnl/vnl_matrix.h"
#include "itkArray.h"
#include "itkMath.h"

namespace itk
{
/** \class VariableSizeMatrix
 * \brief A templated class holding a M x N size Matrix.
 *
 * This class contains a vnl_matrix in order
 * to make all the vnl mathematical methods available. This class is meant to be
 * used when the matrix length cannot be determined at compile time.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */

template< typename T >
class ITK_TEMPLATE_EXPORT VariableSizeMatrix
{
public:
  /** Standard class typedefs. */
  typedef VariableSizeMatrix Self;

  /** Component value type */
  typedef T ValueType;
  typedef T ComponentType;

  /** Internal matrix type */
  typedef vnl_matrix< T > InternalMatrixType;

  /** Matrix by Vector multiplication.  */
  Array< T > operator *(const Array< T > & vector) const;

  /** Matrix by Matrix multiplication.  */
  Self operator *(const Self & matrix) const;

  /** Matrix addition.  */
  Self operator+(const Self & matrix) const;

  const Self & operator+=(const Self & matrix);

  /** Matrix addition.  */
  Self operator-(const Self & matrix) const;

  const Self & operator-=(const Self & matrix);

  /** negation operator */
  Self & operator-();

  /** Matrix by vnl_matrix multiplication.  */
  vnl_matrix< T > operator *(const vnl_matrix< T > & matrix) const;

  /** Matrix by Matrix multiplication.  */
  void operator*=(const Self & matrix);

  /** Matrix by vnl_matrix multiplication.  */
  void operator*=(const vnl_matrix< T > & matrix);

  /** Matrix by vnl_vector multiplication.  */
  vnl_vector< T > operator *(const vnl_vector< T > & matrix) const;

  /** Matrix by scalar multiplication.  */
  void operator*=(const T & value) { m_Matrix *= value; }

  /** Matrix by scalar multiplication.  */
  Self operator*(const T & value)
  {
    Self result(*this);

    result *= value;
    return result;
  }

  /** Matrix by scalar division.  */
  void operator/=(const T & value) { m_Matrix /= value; }

  /** Matrix by scalar division.  */
  Self operator/(const T & value)
  {
    Self result(*this);

    result /= value;
    return result;
  }

  /** Return an element of the matrix. */
  inline T & operator()(unsigned int row, unsigned int col)
  {
    return m_Matrix(row, col);
  }

  /** Return an element of the matrix. */
  inline const T & operator()(unsigned int row, unsigned int col) const
  {
    return m_Matrix(row, col);
  }

  /** Return a row of the matrix. */
  inline T * operator[](unsigned int i)
  {
    return m_Matrix[i];
  }

  /** Return a row of the matrix. */
  inline const T * operator[](unsigned int i) const
  {
    return m_Matrix[i];
  }

  /** Return the matrix. */
  inline InternalMatrixType & GetVnlMatrix(void)
  {
    return m_Matrix;
  }

  /** Return the matrix. */
  inline const InternalMatrixType & GetVnlMatrix(void) const
  {
    return m_Matrix;
  }

  /** Set the matrix to identity. */
  inline void SetIdentity(void)
  {
    m_Matrix.set_identity();
  }

  /** Fill the matrix with a value. */
  inline void Fill(const T & value)
  {
    m_Matrix.fill(value);
  }

  /** Assignment operator. */
  inline const Self & operator=(const vnl_matrix< T > & matrix)
  {
    m_Matrix = matrix;
    return *this;
  }

  /** Comparison operators. */
  inline bool operator==(const Self & matrix) const;

  inline bool operator!=(const Self & matrix) const
  {
    return !this->operator==(matrix);
  }

  /** Assignment operator. */
  inline const Self & operator=(const Self & matrix)
  {
    m_Matrix = matrix.m_Matrix;
    return *this;
  }

  /** Return the inverse matrix. */
  inline vnl_matrix< T > GetInverse(void) const
  {
    vnl_matrix< T > temp = vnl_matrix_inverse< T >(m_Matrix);
    return temp;
  }

  /** Return the transposed matrix. */
  inline vnl_matrix< T > GetTranspose(void) const
  {
    return m_Matrix.transpose();
  }

  /** Default constructor. */
  VariableSizeMatrix():m_Matrix() {}

  VariableSizeMatrix(unsigned int rows, unsigned int cols);

  /** Copy constructor. */
  VariableSizeMatrix(const Self & matrix):m_Matrix(matrix.m_Matrix) {}

  /** Return number of rows in the matrix */
  inline unsigned int Rows() const { return m_Matrix.rows(); }

  /** Return number of columns in the matrix */
  inline unsigned int Cols() const { return m_Matrix.cols(); }

  /** Set the matrix size. Old data lost. Returns true if size changed. */
  inline bool SetSize(unsigned int r, unsigned int c) { return m_Matrix.set_size(r, c); }

private:
  InternalMatrixType m_Matrix;
};

template< typename T >
std::ostream & operator<<(std::ostream & os,
                                     const VariableSizeMatrix< T > & v)
{
  os << v.GetVnlMatrix(); return os;
}

/**
 *  Comparison
 */
template< typename T >
inline
bool
VariableSizeMatrix< T >
::operator==(const Self & matrix) const
{
  if ( ( matrix.Rows() != this->Rows() )
       || ( matrix.Cols() != this->Cols() ) )
    {
    return false;
    }
  bool equal = true;

  for ( unsigned int r = 0; r < this->Rows(); r++ )
    {
    for ( unsigned int c = 0; c < this->Cols(); c++ )
      {
      if ( Math::NotExactlyEquals(m_Matrix(r, c), matrix.m_Matrix(r, c)) )
        {
        equal = false;
        break;
        }
      }
    }
  return equal;
}
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVariableSizeMatrix.hxx"
#endif

#endif
