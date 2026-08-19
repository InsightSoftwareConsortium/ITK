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
#ifndef itkBridgeMathLDLT_h
#define itkBridgeMathLDLT_h

#include "itkMacro.h"
#include "itkArray.h"
#include "itkArray2D.h"
#include "itkMatrix.h"
#include "itkVector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector_fixed.h"

#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

#include <limits>

/** Capability macro. Downstream code selects the Eigen-backed symmetric solve
 * with, e.g.:
 * \code
 *   #if __has_include(<itkBridgeMathLDLT.h>)
 *   #  include <itkBridgeMathLDLT.h>
 *   #endif
 *   #ifdef ITK_BRIDGE_MATH_HAS_SOLVE_SYMMETRIC
 *     x = itk::bridge::Math::SolveSymmetric(A, b);        // itk::Array2D, itk::Array
 *   #else
 *     x = vnl_cholesky(A.as_ref()).solve(b);      // legacy vnl fallback
 *   #endif
 * \endcode
 */
#define ITK_BRIDGE_MATH_HAS_SOLVE_SYMMETRIC 1

namespace itk::bridge
{
namespace Math
{
namespace detail
{
// Solve the symmetric system A x = b via Eigen's robust LDLT (Bunch-Kaufman
// pivoting). A is assumed symmetric; column-major mapping of ITK's row-major
// storage is exact for a symmetric matrix (A == A^T). Throws on a non-finite
// input.
template <typename TReal>
void
SolveSymmetricLDLTEigen(const TReal * aData, const TReal * bData, unsigned int n, TReal * xData)
{
  using ColMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic>;
  using Vector = Eigen::Matrix<TReal, Eigen::Dynamic, 1>;

  const Eigen::Map<const ColMajor> aMap(aData, n, n);
  const Eigen::Map<const Vector>   bMap(bData, n);

  const Eigen::LDLT<ColMajor> ldlt(aMap);
  if (ldlt.info() != Eigen::Success)
  {
    itkGenericExceptionMacro("itk::bridge::Math::SolveSymmetric failed; input is likely non-finite (NaN/Inf).");
  }
  Eigen::Map<Vector>(xData, n) = ldlt.solve(bMap);
}

// Solve A X = B for symmetric A (n x n) and B (n x m) with a SINGLE LDLT
// factorization (all columns solved together). A is symmetric so its col-major
// map of ITK's row-major storage is exact; B and X are mapped row-major. Throws
// on a non-finite input.
template <typename TReal>
void
SolveSymmetricMatrixLDLTEigen(const TReal * aData, const TReal * bData, unsigned int n, unsigned int m, TReal * xData)
{
  using ColMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic>;
  using RowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;

  const Eigen::Map<const ColMajor> aMap(aData, n, n);
  const Eigen::Map<const RowMajor> bMap(bData, n, m);

  const Eigen::LDLT<ColMajor> ldlt(aMap);
  if (ldlt.info() != Eigen::Success)
  {
    itkGenericExceptionMacro("itk::bridge::Math::SolveSymmetric failed; input is likely non-finite (NaN/Inf).");
  }
  Eigen::Map<RowMajor>(xData, n, m) = ldlt.solve(bMap);
}

// Invert symmetric A via LDLT (solves A X = I). Eigen's LDLT::solve silently
// pseudo-inverts zero pivots, so singularity is rejected here from vectorD().
template <typename TReal>
void
InverseSymmetricLDLTEigen(const TReal * aData, unsigned int n, TReal * invData)
{
  using ColMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic>;
  using RowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;

  const Eigen::Map<const ColMajor> aMap(aData, n, n);

  const Eigen::LDLT<ColMajor> ldlt(aMap);
  if (ldlt.info() != Eigen::Success)
  {
    itkGenericExceptionMacro("itk::bridge::Math::InverseSymmetric failed; input is likely non-finite (NaN/Inf).");
  }
  const auto  dAbs = ldlt.vectorD().cwiseAbs().eval();
  const TReal dMax = dAbs.maxCoeff();
  if (dMax == TReal{ 0 } || dAbs.minCoeff() <= dMax * static_cast<TReal>(n) * std::numeric_limits<TReal>::epsilon())
  {
    itkGenericExceptionMacro("itk::bridge::Math::InverseSymmetric failed; input matrix is singular.");
  }
  Eigen::Map<RowMajor>(invData, n, n) = ldlt.solve(ColMajor::Identity(n, n));
}
} // namespace detail

/** \brief Solve the symmetric linear system \c A x = b via LDLT, backed by Eigen.
 *
 * Eigen-backed alternative to vnl_cholesky for a symmetric \a A (positive
 * definite or indefinite). Uses Eigen's robust LDLT (Bunch-Kaufman symmetric
 * pivoting), which handles near-singular and indefinite symmetric systems
 * gracefully at Cholesky-class cost -- so a single call replaces the common
 * cholesky-plus-SVD-fallback idiom. Only \a A's symmetry is assumed; the caller
 * supplies a symmetric matrix.
 *
 * The interface uses ITK container types (no vnl or Eigen type appears in the
 * signature): a runtime-sized \c itk::Array2D / \c itk::Array pair, or a
 * fixed-size \c itk::Matrix / \c itk::Vector pair.
 *
 * A non-finite input throws an itk::ExceptionObject.
 *
 * \ingroup ITKCommon
 */
template <typename TReal>
Array<TReal>
SolveSymmetric(const Array2D<TReal> & A, const Array<TReal> & b)
{
  const unsigned int n = A.rows();
  if (n == 0 || A.cols() != n || b.size() != n)
  {
    itkGenericExceptionMacro("itk::bridge::Math::SolveSymmetric requires a non-empty square A and a matching b.");
  }
  Array<TReal> x(n);
  detail::SolveSymmetricLDLTEigen<TReal>(A.data_block(), b.data_block(), n, x.data_block());
  return x;
}

/** Fixed-size symmetric solve A x = b via LDLT. */
template <typename TReal, unsigned int VDim>
Vector<TReal, VDim>
SolveSymmetric(const Matrix<TReal, VDim, VDim> & A, const Vector<TReal, VDim> & b)
{
  Vector<TReal, VDim> x;
  detail::SolveSymmetricLDLTEigen<TReal>(A.GetVnlMatrix().data_block(), b.GetDataPointer(), VDim, x.GetDataPointer());
  return x;
}

// --- vnl convenience overloads ---------------------------------------------
// Provided so consumers that still hold vnl types (e.g. legacy filters) can call
// directly without wrapping. The ITK-typed overloads above are the forward-
// looking interface; the very-long-term goal is to eliminate vnl from the ITK
// API, so prefer the ITK-typed signatures in new code.

/** Runtime-sized symmetric solve on vnl types (convenience). */
template <typename TReal>
vnl_vector<TReal>
SolveSymmetric(const vnl_matrix<TReal> & A, const vnl_vector<TReal> & b)
{
  const unsigned int n = A.rows();
  if (n == 0 || A.cols() != n || b.size() != n)
  {
    itkGenericExceptionMacro("itk::bridge::Math::SolveSymmetric requires a non-empty square A and a matching b.");
  }
  vnl_vector<TReal> x(n);
  detail::SolveSymmetricLDLTEigen<TReal>(A.data_block(), b.data_block(), n, x.data_block());
  return x;
}

/** Fixed-size symmetric solve on vnl types (convenience). */
template <typename TReal, unsigned int VDim>
vnl_vector_fixed<TReal, VDim>
SolveSymmetric(const vnl_matrix_fixed<TReal, VDim, VDim> & A, const vnl_vector_fixed<TReal, VDim> & b)
{
  vnl_vector_fixed<TReal, VDim> x;
  detail::SolveSymmetricLDLTEigen<TReal>(A.data_block(), b.data_block(), VDim, x.data_block());
  return x;
}

// --- multi-RHS solve and symmetric inverse ---------------------------------

/** Solve the symmetric system \c A X = B for a matrix right-hand side \a B via a
 * single LDLT factorization (all columns solved together -- O(n^3), not one
 * factorization per column). \a A must be symmetric. */
template <typename TReal>
Array2D<TReal>
SolveSymmetric(const Array2D<TReal> & A, const Array2D<TReal> & B)
{
  const unsigned int n = A.rows();
  if (n == 0 || A.cols() != n || B.rows() != n)
  {
    itkGenericExceptionMacro("itk::bridge::Math::SolveSymmetric requires a non-empty square A and a matching B.");
  }
  Array2D<TReal> X(n, B.cols());
  detail::SolveSymmetricMatrixLDLTEigen<TReal>(A.data_block(), B.data_block(), n, B.cols(), X.data_block());
  return X;
}

/** \brief Inverse of a symmetric matrix, backed by Eigen LDLT.
 *
 * Returns \c A^-1 for a symmetric \a A via a single LDLT factorization (solves
 * A X = I). Prefer the solve overloads when only \c A^-1 b or \c A^-1 B is
 * needed -- forming the full inverse is rarely necessary. A non-finite or
 * singular input throws an itk::ExceptionObject.
 *
 * \ingroup ITKCommon
 */
template <typename TReal>
Array2D<TReal>
InverseSymmetric(const Array2D<TReal> & A)
{
  const unsigned int n = A.rows();
  if (n == 0 || A.cols() != n)
  {
    itkGenericExceptionMacro("itk::bridge::Math::InverseSymmetric requires a non-empty square A.");
  }
  Array2D<TReal> inv(n, n);
  detail::InverseSymmetricLDLTEigen<TReal>(A.data_block(), n, inv.data_block());
  return inv;
}

/** Multi-RHS symmetric solve on vnl types (convenience). */
template <typename TReal>
vnl_matrix<TReal>
SolveSymmetric(const vnl_matrix<TReal> & A, const vnl_matrix<TReal> & B)
{
  const unsigned int n = A.rows();
  if (n == 0 || A.cols() != n || B.rows() != n)
  {
    itkGenericExceptionMacro("itk::bridge::Math::SolveSymmetric requires a non-empty square A and a matching B.");
  }
  vnl_matrix<TReal> X(n, B.cols());
  detail::SolveSymmetricMatrixLDLTEigen<TReal>(A.data_block(), B.data_block(), n, B.cols(), X.data_block());
  return X;
}

/** Symmetric inverse on vnl types (convenience). */
template <typename TReal>
vnl_matrix<TReal>
InverseSymmetric(const vnl_matrix<TReal> & A)
{
  const unsigned int n = A.rows();
  if (n == 0 || A.cols() != n)
  {
    itkGenericExceptionMacro("itk::bridge::Math::InverseSymmetric requires a non-empty square A.");
  }
  vnl_matrix<TReal> inv(n, n);
  detail::InverseSymmetricLDLTEigen<TReal>(A.data_block(), n, inv.data_block());
  return inv;
}

} // namespace Math
} // namespace itk::bridge

#endif // itkBridgeMathLDLT_h
