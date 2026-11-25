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
#ifndef itkMatrixExponential_h
#define itkMatrixExponential_h

#include "itkMatrix.h"
#include "itk_eigen.h"
// Eigen's matrix exponential lives in the unsupported MatrixFunctions module.
#include ITK_EIGEN_UNSUPPORTED(MatrixFunctions)

namespace itk
{
namespace Math
{

/** \brief Matrix exponential exp(A) for a real square matrix, backed by Eigen.
 *
 * Replacement for the removed vnl_matrix_exp. Delegates to Eigen's
 * MatrixBase::exp(), which uses scaling-and-squaring with a degree-13 Pade
 * approximant (Higham, SIAM J. Matrix Anal. Appl. 26(4), 2005) -- more robust
 * than the truncated Taylor series formerly provided by vnl_matrix_exp.
 */
namespace detail
{
// VNL/itk::Matrix storage is row-major and contiguous, so map the input/output
// blocks directly instead of copying element-by-element. Compile-time
// dimensions let Eigen size its exp() temporaries on the stack.
template <unsigned int VRows, unsigned int VColumns, typename TReal>
void
MatrixExponentialEigen(const TReal * inData, TReal * outData)
{
  using RowMajorMatrix = Eigen::Matrix<TReal, VRows, VColumns, Eigen::RowMajor>;
  Eigen::Map<const RowMajorMatrix> inMap(inData);
  Eigen::Map<RowMajorMatrix>       outMap(outData);
  outMap = inMap.exp();
}

template <typename TReal>
void
MatrixExponentialEigen(const TReal * inData, TReal * outData, unsigned int n)
{
  using RowMajorMatrix = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
  Eigen::Map<const RowMajorMatrix> inMap(inData, n, n);
  Eigen::Map<RowMajorMatrix>       outMap(outData, n, n);
  outMap = inMap.exp();
}
} // namespace detail

/** Matrix exponential of an itk::Matrix (square). */
template <typename T, unsigned int VRows, unsigned int VColumns>
Matrix<T, VRows, VColumns>
MatrixExponential(const Matrix<T, VRows, VColumns> & A)
{
  static_assert(VRows == VColumns, "MatrixExponential requires a square matrix");
  Matrix<T, VRows, VColumns> result;
  detail::MatrixExponentialEigen<VRows, VColumns>(A.GetVnlMatrix().data_block(), result.GetVnlMatrix().data_block());
  return result;
}

/** Matrix exponential of a vnl_matrix_fixed (square). */
template <typename T, unsigned int VRows, unsigned int VColumns>
vnl_matrix_fixed<T, VRows, VColumns>
MatrixExponential(const vnl_matrix_fixed<T, VRows, VColumns> & A)
{
  static_assert(VRows == VColumns, "MatrixExponential requires a square matrix");
  vnl_matrix_fixed<T, VRows, VColumns> result;
  detail::MatrixExponentialEigen<VRows, VColumns>(A.data_block(), result.data_block());
  return result;
}

/** Matrix exponential of a dynamically-sized vnl_matrix (must be square). */
template <typename T>
vnl_matrix<T>
MatrixExponential(const vnl_matrix<T> & A)
{
  itkAssertOrThrowMacro(A.rows() == A.cols(), "MatrixExponential requires a square matrix");
  const unsigned int n = A.rows();
  vnl_matrix<T>      result(n, n);
  detail::MatrixExponentialEigen<T>(A.data_block(), result.data_block(), n);
  return result;
}

} // namespace Math
} // namespace itk

#endif // itkMatrixExponential_h
