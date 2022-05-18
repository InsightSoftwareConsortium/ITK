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
#ifndef itkMatrix_hxx
#define itkMatrix_hxx

#include "itkNumericTraits.h"

namespace itk
{
/**
 *  Product by a Vector
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
Vector<T, VRows> Matrix<T, VRows, VColumns>::operator*(const Vector<T, VColumns> & vect) const
{
  Vector<T, VRows> result;
  for (unsigned int r = 0; r < VRows; ++r)
  {
    T sum = NumericTraits<T>::ZeroValue();
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      sum += m_Matrix(r, c) * vect[c];
    }
    result[r] = sum;
  }
  return result;
}

/**
 *  Product by a Point
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
Point<T, VRows> Matrix<T, VRows, VColumns>::operator*(const Point<T, VColumns> & pnt) const
{
  Point<T, VRows> result;
  for (unsigned int r = 0; r < VRows; ++r)
  {
    T sum = NumericTraits<T>::ZeroValue();
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      sum += m_Matrix(r, c) * pnt[c];
    }
    result[r] = sum;
  }
  return result;
}

/**
 *  Product by a vnl_vector_fixed
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
vnl_vector_fixed<T, VRows> Matrix<T, VRows, VColumns>::operator*(const vnl_vector_fixed<T, VColumns> & inVNLvect) const
{
  vnl_vector_fixed<T, VRows> result;
  for (unsigned int r = 0; r < VRows; ++r)
  {
    T sum = NumericTraits<T>::ZeroValue();
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      sum += m_Matrix(r, c) * inVNLvect[c];
    }
    result[r] = sum;
  }
  return result;
}

/**
 *  Product by a CovariantVector
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
CovariantVector<T, VRows> Matrix<T, VRows, VColumns>::operator*(const CovariantVector<T, VColumns> & covect) const
{
  CovariantVector<T, VRows> result;
  for (unsigned int r = 0; r < VRows; ++r)
  {
    T sum = NumericTraits<T>::ZeroValue();
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      sum += m_Matrix(r, c) * covect[c];
    }
    result[r] = sum;
  }
  return result;
}

/**
 *  Product by a matrix
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
Matrix<T, VRows, VColumns> Matrix<T, VRows, VColumns>::operator*(const CompatibleSquareMatrixType & matrix) const
{
  const Self result(m_Matrix * matrix.GetVnlMatrix());
  return result;
}

/**
 *  Matrix Addition
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
Matrix<T, VRows, VColumns>
Matrix<T, VRows, VColumns>::operator+(const Self & matrix) const
{
  Self result;

  for (unsigned int r = 0; r < VRows; ++r)
  {
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      result.m_Matrix(r, c) = m_Matrix(r, c) + matrix.m_Matrix(r, c);
    }
  }
  return result;
}

/**
 *  Matrix Addition in-place
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
const Matrix<T, VRows, VColumns> &
Matrix<T, VRows, VColumns>::operator+=(const Self & matrix)
{
  for (unsigned int r = 0; r < VRows; ++r)
  {
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      m_Matrix(r, c) += matrix.m_Matrix(r, c);
    }
  }
  return *this;
}

/**
 *  Matrix Subtraction
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
Matrix<T, VRows, VColumns>
Matrix<T, VRows, VColumns>::operator-(const Self & matrix) const
{
  Self result;

  for (unsigned int r = 0; r < VRows; ++r)
  {
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      result.m_Matrix(r, c) = m_Matrix(r, c) - matrix.m_Matrix(r, c);
    }
  }
  return result;
}

/**
 *  Matrix subtraction in-place
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
const Matrix<T, VRows, VColumns> &
Matrix<T, VRows, VColumns>::operator-=(const Self & matrix)
{
  for (unsigned int r = 0; r < VRows; ++r)
  {
    for (unsigned int c = 0; c < VColumns; ++c)
    {
      m_Matrix(r, c) -= matrix.m_Matrix(r, c);
    }
  }
  return *this;
}

/**
 *  Product by a vnl_matrix
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
vnl_matrix<T> Matrix<T, VRows, VColumns>::operator*(const vnl_matrix<T> & matrix) const
{
  return m_Matrix * matrix;
}

/**
 *  Product by a matrix
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
void
Matrix<T, VRows, VColumns>::operator*=(const CompatibleSquareMatrixType & matrix)
{
  m_Matrix *= matrix.GetVnlMatrix();
}

/**
 *  Product by a vnl_matrix
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
void
Matrix<T, VRows, VColumns>::operator*=(const vnl_matrix<T> & matrix)
{
  m_Matrix *= matrix;
}

/**
 *  Product by a vnl_vector
 */
template <typename T, unsigned int VRows, unsigned int VColumns>
vnl_vector<T> Matrix<T, VRows, VColumns>::operator*(const vnl_vector<T> & vc) const
{
  return m_Matrix * vc;
}
} // end namespace itk

#endif
