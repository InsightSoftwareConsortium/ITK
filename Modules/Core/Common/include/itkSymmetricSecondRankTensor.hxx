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
#ifndef itkSymmetricSecondRankTensor_hxx
#define itkSymmetricSecondRankTensor_hxx

#include "itkNumericTraitsTensorPixel.h"

namespace itk
{
/**
 * Assignment Operator from a scalar constant
 */
template <typename T, unsigned int VDimension>
SymmetricSecondRankTensor<T, VDimension> &
SymmetricSecondRankTensor<T, VDimension>::operator=(const ComponentType & r)
{
  BaseArray::operator=(&r);
  return *this;
}

/**
 * Assignment from a plain array
 */
template <typename T, unsigned int VDimension>
SymmetricSecondRankTensor<T, VDimension> &
SymmetricSecondRankTensor<T, VDimension>::operator=(const ComponentArrayType r)
{
  BaseArray::operator=(r);
  return *this;
}

/**
 * Returns a temporary copy of a vector
 */
template <typename T, unsigned int VDimension>
SymmetricSecondRankTensor<T, VDimension>
SymmetricSecondRankTensor<T, VDimension>::operator+(const Self & r) const
{
  Self result;

  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    result[i] = (*this)[i] + r[i];
  }
  return result;
}

/**
 * Returns a temporary copy of a vector
 */
template <typename T, unsigned int VDimension>
SymmetricSecondRankTensor<T, VDimension>
SymmetricSecondRankTensor<T, VDimension>::operator-(const Self & r) const
{
  Self result;

  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    result[i] = (*this)[i] - r[i];
  }
  return result;
}

/**
 * Performs addition in place
 */
template <typename T, unsigned int VDimension>
const SymmetricSecondRankTensor<T, VDimension> &
SymmetricSecondRankTensor<T, VDimension>::operator+=(const Self & r)
{
  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    (*this)[i] += r[i];
  }
  return *this;
}

/**
 * Performs subtraction in place
 */
template <typename T, unsigned int VDimension>
const SymmetricSecondRankTensor<T, VDimension> &
SymmetricSecondRankTensor<T, VDimension>::operator-=(const Self & r)
{
  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    (*this)[i] -= r[i];
  }
  return *this;
}

/**
 * Performs multiplication by a scalar, in place
 */
template <typename T, unsigned int VDimension>
const SymmetricSecondRankTensor<T, VDimension> &
SymmetricSecondRankTensor<T, VDimension>::operator*=(const RealValueType & r)
{
  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    (*this)[i] *= r;
  }
  return *this;
}

/**
 * Performs division by a scalar, in place
 */
template <typename T, unsigned int VDimension>
const SymmetricSecondRankTensor<T, VDimension> &
SymmetricSecondRankTensor<T, VDimension>::operator/=(const RealValueType & r)
{
  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    (*this)[i] /= r;
  }
  return *this;
}

/**
 * Performs multiplication with a scalar
 */
template <typename T, unsigned int VDimension>
SymmetricSecondRankTensor<T, VDimension> SymmetricSecondRankTensor<T, VDimension>::operator*(
  const RealValueType & r) const
{
  Self result;

  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    result[i] = (*this)[i] * r;
  }
  return result;
}

/**
 * Performs division by a scalar
 */
template <typename T, unsigned int VDimension>
SymmetricSecondRankTensor<T, VDimension>
SymmetricSecondRankTensor<T, VDimension>::operator/(const RealValueType & r) const
{
  Self result;

  for (unsigned int i = 0; i < InternalDimension; ++i)
  {
    result[i] = (*this)[i] / r;
  }
  return result;
}

/**
 * Matrix notation access to elements
 */
template <typename T, unsigned int VDimension>
auto
SymmetricSecondRankTensor<T, VDimension>::operator()(unsigned int row, unsigned int col) const -> const ValueType &
{
  unsigned int k;

  if (row < col)
  {
    k = row * Dimension + col - row * (row + 1) / 2;
  }
  else
  {
    k = col * Dimension + row - col * (col + 1) / 2;
  }

  if (k >= InternalDimension)
  {
    k = 0;
  }

  return (*this)[k];
}

/**
 * Matrix notation access to elements
 */
template <typename T, unsigned int VDimension>
auto
SymmetricSecondRankTensor<T, VDimension>::operator()(unsigned int row, unsigned int col) -> ValueType &
{
  unsigned int k;

  if (row < col)
  {
    k = row * Dimension + col - row * (row + 1) / 2;
  }
  else
  {
    k = col * Dimension + row - col * (col + 1) / 2;
  }

  if (k >= InternalDimension)
  {
    k = 0;
  }

  return (*this)[k];
}

/**
 * Set the Tensor to an Identity.
 * Set ones in the diagonal and zeroes every where else.
 */
template <typename T, unsigned int VDimension>
void
SymmetricSecondRankTensor<T, VDimension>::SetIdentity()
{
  this->Fill(NumericTraits<T>::ZeroValue());
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    (*this)(i, i) = NumericTraits<T>::OneValue();
  }
}

/**
 * Get the Trace
 */
template <typename T, unsigned int VDimension>
auto
SymmetricSecondRankTensor<T, VDimension>::GetTrace() const -> AccumulateValueType
{
  AccumulateValueType trace = NumericTraits<AccumulateValueType>::ZeroValue();
  unsigned int        k = 0;

  for (unsigned int i = 0; i < Dimension; ++i)
  {
    trace += (*this)[k];
    k += (Dimension - i);
  }
  return trace;
}

/**
 * Compute Eigen Values
 */
template <typename T, unsigned int VDimension>
void
SymmetricSecondRankTensor<T, VDimension>::ComputeEigenValues(EigenValuesArrayType & eigenValues) const
{
  SymmetricEigenAnalysisType symmetricEigenSystem;

  MatrixType tensorMatrix;

  for (unsigned int row = 0; row < Dimension; ++row)
  {
    for (unsigned int col = 0; col < Dimension; ++col)
    {
      tensorMatrix[row][col] = (*this)(row, col);
    }
  }

  symmetricEigenSystem.ComputeEigenValues(tensorMatrix, eigenValues);
}

/**
 * Compute Eigen analysis, it returns an array with eigen values
 * and a Matrix with eigen vectors
 */
template <typename T, unsigned int VDimension>
void
SymmetricSecondRankTensor<T, VDimension>::ComputeEigenAnalysis(EigenValuesArrayType &   eigenValues,
                                                               EigenVectorsMatrixType & eigenVectors) const
{
  SymmetricEigenAnalysisType symmetricEigenSystem;

  MatrixType tensorMatrix;

  for (unsigned int row = 0; row < Dimension; ++row)
  {
    for (unsigned int col = 0; col < Dimension; ++col)
    {
      tensorMatrix[row][col] = (*this)(row, col);
    }
  }

  symmetricEigenSystem.ComputeEigenValuesAndVectors(tensorMatrix, eigenValues, eigenVectors);
}

/**
 * Set the Tensor to a Rotated version of the current tensor.
 * matrix * self * Transpose(matrix)
 *
 */
template <typename T, unsigned int VDimension>
template <typename TMatrixValueType>
SymmetricSecondRankTensor<T, VDimension>
SymmetricSecondRankTensor<T, VDimension>::Rotate(const Matrix<TMatrixValueType, VDimension, VDimension> & m) const
{
  Self result;
  using RotationMatrixType = Matrix<double, VDimension, VDimension>;
  RotationMatrixType SCT; // self * Transpose(m)
  for (unsigned int r = 0; r < VDimension; ++r)
  {
    for (unsigned int c = 0; c < VDimension; ++c)
    {
      double sum = 0.0;
      for (unsigned int t = 0; t < VDimension; ++t)
      {
        sum += (*this)(r, t) * m(c, t);
      }
      SCT(r, c) = sum;
    }
  }
  // self = m * sct;
  for (unsigned int r = 0; r < VDimension; ++r)
  {
    for (unsigned int c = 0; c < VDimension; ++c)
    {
      double sum = 0.0;
      for (unsigned int t = 0; t < VDimension; ++t)
      {
        sum += m(r, t) * SCT(t, c);
      }
      (result)(r, c) = static_cast<T>(sum);
    }
  }
  return result;
}

/**
 * Pre-multiply the Tensor by a Matrix
 */
template <typename T, unsigned int VDimension>
auto
SymmetricSecondRankTensor<T, VDimension>::PreMultiply(const MatrixType & m) const -> MatrixType
{
  MatrixType result;

  using AccumulateType = typename NumericTraits<T>::AccumulateType;
  for (unsigned int r = 0; r < VDimension; ++r)
  {
    for (unsigned int c = 0; c < VDimension; ++c)
    {
      AccumulateType sum = NumericTraits<AccumulateType>::ZeroValue();
      for (unsigned int t = 0; t < VDimension; ++t)
      {
        sum += m(r, t) * (*this)(t, c);
      }
      result(r, c) = static_cast<T>(sum);
    }
  }
  return result;
}

/**
 * Post-multiply the Tensor by a Matrix
 */
template <typename T, unsigned int VDimension>
auto
SymmetricSecondRankTensor<T, VDimension>::PostMultiply(const MatrixType & m) const -> MatrixType
{
  MatrixType result;

  using AccumulateType = typename NumericTraits<T>::AccumulateType;
  for (unsigned int r = 0; r < VDimension; ++r)
  {
    for (unsigned int c = 0; c < VDimension; ++c)
    {
      AccumulateType sum = NumericTraits<AccumulateType>::ZeroValue();
      for (unsigned int t = 0; t < VDimension; ++t)
      {
        sum += (*this)(r, t) * m(t, c);
      }
      result(r, c) = static_cast<T>(sum);
    }
  }
  return result;
}

/**
 * Print content to an ostream
 */
template <typename T, unsigned int VDimension>
std::ostream &
operator<<(std::ostream & os, const SymmetricSecondRankTensor<T, VDimension> & c)
{
  for (unsigned int i = 0; i < c.GetNumberOfComponents(); ++i)
  {
    os << static_cast<typename NumericTraits<T>::PrintType>(c[i]) << "  ";
  }
  return os;
}

/**
 * Read content from an istream
 */
template <typename T, unsigned int VDimension>
std::istream &
operator>>(std::istream & is, SymmetricSecondRankTensor<T, VDimension> & dt)
{
  for (unsigned int i = 0; i < dt.GetNumberOfComponents(); ++i)
  {
    is >> dt[i];
  }
  return is;
}
} // end namespace itk

#endif
