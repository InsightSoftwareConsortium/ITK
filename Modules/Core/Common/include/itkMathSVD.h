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
#ifndef itkMathSVD_h
#define itkMathSVD_h

#include "itkMacro.h"
#include "itkMatrix.h"
#include "itkEigenDecompositionSignConvention.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector_fixed.h"

#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

#include <limits>

namespace itk
{
namespace Math
{

namespace detail
{
// rcond < 0 selects an automatic relative threshold of n * epsilon.
template <typename TReal>
TReal
ResolveRcond(TReal rcond, unsigned int n)
{
  return rcond < TReal{ 0 } ? static_cast<TReal>(n) * std::numeric_limits<TReal>::epsilon() : rcond;
}

// Moore-Penrose pseudo-inverse V diag(1/w) U^T, with singular values at or below
// rcond*max(w) treated as zero. Works for vnl_matrix and vnl_matrix_fixed.
template <typename TMatrix, typename TVector, typename TReal>
TMatrix
PseudoInverse(const TMatrix & U, const TVector & W, const TMatrix & V, TReal rcond)
{
  const unsigned int k = W.size();
  // W is sorted descending (Eigen guarantee), so W[0] is the max without an O(k) scan.
  const TReal tol = ResolveRcond(rcond, k) * W[0];
  TMatrix     scaledV = V;
  for (unsigned int col = 0; col < k; ++col)
  {
    const TReal s = (W[col] > tol) ? TReal{ 1 } / W[col] : TReal{ 0 };
    for (unsigned int i = 0; i < scaledV.rows(); ++i)
    {
      scaledV(i, col) *= s;
    }
  }
  return scaledV * U.transpose();
}

// Least-squares / minimum-norm solution x = V diag(1/w) U^T b of A x = b.
template <typename TMatrix, typename TVector, typename TReal>
TVector
SolveLinear(const TMatrix & U, const TVector & W, const TMatrix & V, const TVector & b, TReal rcond)
{
  const unsigned int n = W.size();
  const TReal        tol = ResolveRcond(rcond, n) * W[0]; // W sorted descending; W[0] == max
  TVector            utb = U.transpose() * b;
  for (unsigned int k = 0; k < n; ++k)
  {
    utb[k] = (W[k] > tol) ? utb[k] / W[k] : TReal{ 0 };
  }
  return V * utb;
}

template <typename TVector, typename TReal>
unsigned int
NumericalRank(const TVector & W, TReal rcond)
{
  const unsigned int n = W.size();
  const TReal        tol = ResolveRcond(rcond, n) * W[0]; // W sorted descending; W[0] == max
  unsigned int       count = 0;
  for (unsigned int k = 0; k < n; ++k)
  {
    if (W[k] > tol)
    {
      ++count;
    }
  }
  return count;
}

// Reconstruct U diag(w) V^T, treating singular values at or below rcond*max(w) as
// zero (a truncated, rank-reduced reconstruction of A).
template <typename TMatrix, typename TVector, typename TReal>
TMatrix
Recompose(const TMatrix & U, const TVector & W, const TMatrix & V, TReal rcond)
{
  const unsigned int k = W.size();
  const TReal        tol = ResolveRcond(rcond, k) * W[0]; // W sorted descending; W[0] == max
  TMatrix            scaledU = U;
  for (unsigned int col = 0; col < k; ++col)
  {
    const TReal s = (W[col] > tol) ? W[col] : TReal{ 0 };
    for (unsigned int i = 0; i < scaledU.rows(); ++i)
    {
      scaledU(i, col) *= s;
    }
  }
  return scaledU * V.transpose();
}
} // namespace detail

/** Result of a fixed-size square SVD: A == U * diag(W) * V^T, W descending.
 * For all solver methods, \a rcond < 0 auto-selects a VDim*epsilon threshold. */
template <typename TReal, unsigned int VDim>
struct FixedSquareSVDResult
{
  vnl_matrix_fixed<TReal, VDim, VDim> U{};
  vnl_vector_fixed<TReal, VDim>       W{};
  vnl_matrix_fixed<TReal, VDim, VDim> V{};

  /** Moore-Penrose pseudo-inverse A^+. */
  vnl_matrix_fixed<TReal, VDim, VDim>
  PseudoInverse(TReal rcond = TReal{ -1 }) const
  {
    return detail::PseudoInverse(U, W, V, rcond);
  }

  /** Least-squares / minimum-norm solution of A x = b. */
  vnl_vector_fixed<TReal, VDim>
  Solve(const vnl_vector_fixed<TReal, VDim> & b, TReal rcond = TReal{ -1 }) const
  {
    return detail::SolveLinear(U, W, V, b, rcond);
  }

  /** Numerical rank (count of singular values above rcond*max(w)). */
  unsigned int
  Rank(TReal rcond = TReal{ -1 }) const
  {
    return detail::NumericalRank(W, rcond);
  }

  /** Reconstruct A with singular values at or below rcond*max(w) zeroed. */
  vnl_matrix_fixed<TReal, VDim, VDim>
  Recompose(TReal rcond = TReal{ -1 }) const
  {
    return detail::Recompose(U, W, V, rcond);
  }
};

/** Result of a runtime-sized SVD (square or rectangular): A == U diag(W) V^T,
 * W descending. For an m x n input, U is m x k, V is n x k and W has length
 * k = min(m, n) (thin factors). \a rcond < 0 auto-selects a k*epsilon threshold. */
template <typename TReal>
struct SVDResult
{
  vnl_matrix<TReal> U{};
  vnl_vector<TReal> W{};
  vnl_matrix<TReal> V{};

  /** Moore-Penrose pseudo-inverse A^+. */
  vnl_matrix<TReal>
  PseudoInverse(TReal rcond = TReal{ -1 }) const
  {
    return detail::PseudoInverse(U, W, V, rcond);
  }

  /** Least-squares / minimum-norm solution of A x = b. */
  vnl_vector<TReal>
  Solve(const vnl_vector<TReal> & b, TReal rcond = TReal{ -1 }) const
  {
    return detail::SolveLinear(U, W, V, b, rcond);
  }

  /** Numerical rank (count of singular values above rcond*max(w)). */
  unsigned int
  Rank(TReal rcond = TReal{ -1 }) const
  {
    return detail::NumericalRank(W, rcond);
  }

  /** Reconstruct A with singular values at or below rcond*max(w) zeroed. */
  vnl_matrix<TReal>
  Recompose(TReal rcond = TReal{ -1 }) const
  {
    return detail::Recompose(U, W, V, rcond);
  }
};

namespace detail
{
// Above this compile-time size the fixed overload delegates to the runtime path
// (keeps a large VDim off Eigen's stack-allocation limit).
constexpr unsigned int kFixedSVDMaxDim = 16;

// JacobiSVD+NoQRPreconditioner is fastest for small square inputs; BDCSVD is
// faster and more accurate for larger n.
constexpr unsigned int kJacobiMaxDim = 6;

// Runtime-sized square SVD: JacobiSVD + NoQRPreconditioner for small n, BDCSVD for
// larger n where Jacobi sweeps scale poorly. Throws on a failed decomposition.
template <typename TReal>
void
DynamicSquareSVDEigen(const TReal * inData, unsigned int n, TReal * uData, TReal * wData, TReal * vData)
{
  using RowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
  using ColMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic>;
  constexpr int full = Eigen::ComputeFullU | Eigen::ComputeFullV;

  const Eigen::Map<const RowMajor> inMap(inData, n, n);
  Eigen::Map<RowMajor>             uMap(uData, n, n);
  Eigen::Map<RowMajor>             vMap(vData, n, n);

  const auto extract = [&](const auto & svd) {
    if (svd.info() != Eigen::Success)
    {
      itkGenericExceptionMacro("itk::Math::SVD failed; input is likely non-finite (NaN/Inf).");
    }
    uMap = svd.matrixU();
    vMap = svd.matrixV();
    for (unsigned int i = 0; i < n; ++i)
    {
      wData[i] = svd.singularValues()[i];
    }
  };

  if (n <= kJacobiMaxDim)
  {
    extract(Eigen::JacobiSVD < ColMajor, full | Eigen::NoQRPreconditioner > (inMap));
  }
  else
  {
    extract(Eigen::BDCSVD<ColMajor, full>(inMap));
  }
}

// NoQRPreconditioner skips the column-pivot QR (wasted for a square input); large
// VDim falls back to the runtime path. Throws on a failed/non-finite decomposition.
template <unsigned int VDim, typename TReal>
void
SquareSVDEigen(const TReal * inData, TReal * uData, TReal * wData, TReal * vData)
{
  if constexpr (VDim > kFixedSVDMaxDim)
  {
    DynamicSquareSVDEigen<TReal>(inData, VDim, uData, wData, vData);
  }
  else
  {
    // Compile-time sizes ≤ kFixedSVDMaxDim always take JacobiSVD: it stack-allocates,
    // fully unrolls, and beats BDCSVD's dynamic setup at these sizes (kJacobiMaxDim
    // gates only the runtime path, where BDCSVD's allocation pays off for larger n).
    using RowMajor = Eigen::Matrix<TReal, VDim, VDim, Eigen::RowMajor>;
    using ColMajor = Eigen::Matrix<TReal, VDim, VDim>;
    constexpr int options = Eigen::ComputeFullU | Eigen::ComputeFullV | Eigen::NoQRPreconditioner;

    const Eigen::Map<const RowMajor>          inMap(inData);
    const Eigen::JacobiSVD<ColMajor, options> svd(inMap);
    if (svd.info() != Eigen::Success)
    {
      itkGenericExceptionMacro("itk::Math::SVD failed; input is likely non-finite (NaN/Inf).");
    }

    Eigen::Map<RowMajor> uMap(uData);
    Eigen::Map<RowMajor> vMap(vData);
    uMap = svd.matrixU();
    vMap = svd.matrixV();
    for (unsigned int i = 0; i < VDim; ++i)
    {
      wData[i] = svd.singularValues()[i];
    }
  }
}

// Rectangular SVD via BDCSVD with thin U/V (Eigen's general engine for non-square
// inputs). For an m x n input: U is m x k, V is n x k, W has length k = min(m, n).
// Throws on a failed decomposition.
template <typename TReal>
void
RectangularSVDEigen(const TReal * inData,
                    unsigned int  rows,
                    unsigned int  cols,
                    TReal *       uData,
                    TReal *       wData,
                    TReal *       vData)
{
  using RowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
  using ColMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic>;
  constexpr int      thin = Eigen::ComputeThinU | Eigen::ComputeThinV;
  const unsigned int k = (rows < cols) ? rows : cols;

  const Eigen::Map<const RowMajor>    inMap(inData, rows, cols);
  const Eigen::BDCSVD<ColMajor, thin> svd(inMap);
  if (svd.info() != Eigen::Success)
  {
    itkGenericExceptionMacro("itk::Math::SVD failed; input is likely non-finite (NaN/Inf).");
  }
  Eigen::Map<RowMajor> uMap(uData, rows, k);
  Eigen::Map<RowMajor> vMap(vData, cols, k);
  uMap = svd.matrixU();
  vMap = svd.matrixV();
  for (unsigned int i = 0; i < k; ++i)
  {
    wData[i] = svd.singularValues()[i];
  }
}
} // namespace detail

/** \brief Singular value decomposition A = U diag(W) V^T, backed by Eigen.
 *
 * Opt-in Eigen-backed alternative to vnl_svd: the optimized Eigen path is
 * competitive with vnl_svd at the small fixed sizes that dominate ITK usage and
 * at large sizes (via BDCSVD), at equal accuracy. Factors are returned as vnl
 * matrices/vectors, so no Eigen type appears in the interface. The result offers
 * PseudoInverse(), Solve(), Rank() and Recompose(); their default rcond truncates
 * singular values at k*epsilon*max(W), so pass rcond = 0 to keep every nonzero
 * singular value.
 *
 * Fixed compile-time sizes use JacobiSVD with NoQRPreconditioner (valid for
 * square inputs) over a zero-copy Eigen::Map; runtime square sizes select JacobiSVD
 * for small n and BDCSVD for larger n. Runtime rectangular inputs use BDCSVD with
 * thin U/V (the dispatch is a single dimension comparison, so the square fast path
 * is unaffected). Fixed-size overloads are square-only by signature.
 *
 * Singular vectors are defined only up to a sign (and, for repeated singular
 * values, a rotation within the shared subspace). With \a canonicalizeSigns
 * (default) the largest-magnitude entry of each U column is made positive and the
 * paired V column flipped to match, giving a deterministic convention when the
 * singular values are distinct; a degenerate subspace still has an arbitrary
 * basis. Unlike vnl_svd -- whose singular-vector signs depend on solver internals
 * and SIMD width -- this canonicalization makes results reproducible across builds
 * and platforms at negligible cost (under 1% of the decomposition); it is an added
 * benefit of the Eigen-backed path. Pass canonicalizeSigns = false to instead
 * reproduce vnl_svd's raw (non-canonical) signs, e.g. for equivalence testing. A
 * non-finite input throws an itk::ExceptionObject.
 *
 * \ingroup ITKCommon
 */
template <typename TReal, unsigned int VDim>
FixedSquareSVDResult<TReal, VDim>
SVD(const vnl_matrix_fixed<TReal, VDim, VDim> & A, bool canonicalizeSigns = true)
{
  FixedSquareSVDResult<TReal, VDim> result;
  detail::SquareSVDEigen<VDim>(A.data_block(), result.U.data_block(), result.W.data_block(), result.V.data_block());
  if (canonicalizeSigns)
  {
    itk::detail::CanonicalizeColumnSignsPaired(result.U, result.V);
  }
  return result;
}

/** SVD of a fixed-size square itk::Matrix. */
template <typename TReal, unsigned int VDim>
FixedSquareSVDResult<TReal, VDim>
SVD(const Matrix<TReal, VDim, VDim> & A, bool canonicalizeSigns = true)
{
  return SVD<TReal, VDim>(A.GetVnlMatrix(), canonicalizeSigns);
}

/** SVD of a runtime-sized vnl_matrix (square or rectangular). Square inputs take
 * the fast square path (JacobiSVD+NoQRPreconditioner / BDCSVD); rectangular inputs
 * use BDCSVD with thin U/V. The dispatch is a single dimension comparison. */
template <typename TReal>
SVDResult<TReal>
SVD(const vnl_matrix<TReal> & A, bool canonicalizeSigns = true)
{
  const unsigned int rows = A.rows();
  const unsigned int cols = A.cols();
  if (rows == 0 || cols == 0)
  {
    itkGenericExceptionMacro("itk::Math::SVD requires a non-empty matrix.");
  }
  const unsigned int k = (rows < cols) ? rows : cols;
  SVDResult<TReal>   result;
  result.U.set_size(rows, k);
  result.V.set_size(cols, k);
  result.W.set_size(k);
  if (rows == cols)
  {
    detail::DynamicSquareSVDEigen<TReal>(
      A.data_block(), rows, result.U.data_block(), result.W.data_block(), result.V.data_block());
  }
  else
  {
    detail::RectangularSVDEigen<TReal>(
      A.data_block(), rows, cols, result.U.data_block(), result.W.data_block(), result.V.data_block());
  }
  if (canonicalizeSigns)
  {
    itk::detail::CanonicalizeColumnSignsPaired(result.U, result.V);
  }
  return result;
}

} // namespace Math
} // namespace itk

#endif // itkMathSVD_h
