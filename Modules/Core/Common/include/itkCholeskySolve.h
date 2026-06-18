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
#ifndef itkCholeskySolve_h
#define itkCholeskySolve_h

#include "itkMacro.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

namespace itk
{
namespace Math
{

/** \brief Cholesky-based linear algebra for symmetric matrices, backed by Eigen.
 *
 * SolveSymmetricPositiveDefinite() uses Eigen's robust LDL^T (pivoted) factor,
 * which handles near-semidefinite regularized Gram matrices. CholeskyLowerTriangle()
 * returns the L of A = L L^T via Eigen's LLT.
 */
namespace detail
{
// vnl storage is row-major and contiguous, so map the data block directly
// (no copy). For a symmetric A the storage order is immaterial; b/x map as
// plain contiguous vectors.
template <typename TReal>
using CholeskyRowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
template <typename TReal>
using CholeskyVector = Eigen::Matrix<TReal, Eigen::Dynamic, 1>;
} // namespace detail

/** Solve A x = b for a symmetric positive (semi-)definite A, via Eigen LDL^T.
 *  The pivoted LDL^T is robust to rank-deficiency and ill-conditioning. */
template <typename T>
vnl_vector<T>
SolveSymmetricPositiveDefinite(const vnl_matrix<T> & A, const vnl_vector<T> & b)
{
  itkAssertOrThrowMacro(A.rows() == A.cols(), "SolveSymmetricPositiveDefinite requires a square matrix");
  itkAssertOrThrowMacro(A.rows() == b.size(), "SolveSymmetricPositiveDefinite requires matching b length");
  const unsigned int                            n = A.rows();
  Eigen::Map<const detail::CholeskyRowMajor<T>> aMap(A.data_block(), n, n);
  Eigen::Map<const detail::CholeskyVector<T>>   bMap(b.data_block(), n);
  vnl_vector<T>                                 x(n);
  Eigen::Map<detail::CholeskyVector<T>>         xMap(x.data_block(), n);
  xMap = aMap.template selfadjointView<Eigen::Lower>().ldlt().solve(bMap);
  return x;
}

/** Lower-triangular Cholesky factor L of A = L L^T (A symmetric pos. def.),
 *  via Eigen LLT. Mirrors vnl_cholesky::lower_triangle(). */
template <typename T>
vnl_matrix<T>
CholeskyLowerTriangle(const vnl_matrix<T> & A)
{
  itkAssertOrThrowMacro(A.rows() == A.cols(), "CholeskyLowerTriangle requires a square matrix");
  const unsigned int                            n = A.rows();
  Eigen::Map<const detail::CholeskyRowMajor<T>> aMap(A.data_block(), n, n);
  const auto                                    llt = aMap.template selfadjointView<Eigen::Lower>().llt();
  if (llt.info() != Eigen::Success)
  {
    itkGenericExceptionMacro("CholeskyLowerTriangle requires a positive-definite matrix");
  }
  const detail::CholeskyRowMajor<T>       lower = llt.matrixL();
  vnl_matrix<T>                           result(n, n);
  Eigen::Map<detail::CholeskyRowMajor<T>> rMap(result.data_block(), n, n);
  rMap = lower;
  return result;
}

} // namespace Math
} // namespace itk

#endif // itkCholeskySolve_h
