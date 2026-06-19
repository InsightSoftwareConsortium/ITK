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
#ifndef itkSymmetricEigenDecomposition_h
#define itkSymmetricEigenDecomposition_h

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_diag_matrix.h"
#include "itkEigenDecompositionSignConvention.h"
#include "itkEigenDecompositionSolverInfo.h"
#include "itkMacro.h"
#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

namespace itk
{

/** \class SymmetricEigenDecomposition
 * \brief Stored eigendecomposition of a real symmetric matrix, Eigen-backed.
 *
 * Computes A = V D V^T for symmetric A in the constructor and stores the
 * result. Eigenvalues (D) are real and ascending; the columns of V are the
 * orthonormal eigenvectors, sign-canonicalized (each column's largest-magnitude
 * entry is positive) for cross-platform reproducibility. The public V / D
 * members and the get_eigenvector / get_eigenvalue accessors mirror the legacy
 * vnl_symmetric_eigensystem layout, so its call sites port by changing only the
 * type name, with no Eigen type in the interface.
 *
 * This is the supported Eigen-backed replacement for the now-deprecated
 * vnl_symmetric_eigensystem (netlib EISPACK rs), which emits a deprecation
 * warning under ITK_LEGACY_REMOVE and is removed under ITK_FUTURE_LEGACY_REMOVE.
 *
 * \note Distinct from itk::SymmetricEigenAnalysis, which is a configurable
 * solver object (SetDimension / SetOrderEigenValues) that writes results into
 * caller-supplied output arguments and is tuned for repeated per-pixel tensor
 * eigenanalysis; it does not canonicalize eigenvector signs. This class is a
 * single-shot stored decomposition. Prefer SymmetricEigenAnalysis for
 * high-throughput fixed-size tensor work; prefer this for
 * vnl_symmetric_eigensystem-style usage.
 *
 * \sa SymmetricEigenAnalysis
 * \ingroup ITKCommon
 */
template <typename T>
class SymmetricEigenDecomposition
{
public:
  /** Compute the decomposition of symmetric \a M. With \a canonicalizeSigns
   * (the default) each eigenvector column's largest-magnitude entry is made
   * positive for cross-platform reproducibility; pass false to keep the solver's
   * raw signs. */
  explicit SymmetricEigenDecomposition(const vnl_matrix<T> & M, bool canonicalizeSigns = true)
    : V(M.rows(), M.cols())
    , D(M.rows())
  {
    using RowMajor = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    using Vector = Eigen::Matrix<T, Eigen::Dynamic, 1>;
    const unsigned int n = M.rows();

    Eigen::Map<const RowMajor>                    mMap(M.data_block(), n, n);
    const Eigen::SelfAdjointEigenSolver<RowMajor> solver(mMap);

    if (solver.info() != Eigen::Success)
    {
      itkGenericExceptionMacro(<< "SymmetricEigenDecomposition: Eigen SelfAdjointEigenSolver failed: "
                               << detail::EigenComputationInfoString(solver.info()) << '.');
    }

    Eigen::Map<RowMajor>(V.data_block(), n, n) = solver.eigenvectors();
    Eigen::Map<Vector>(D.data_block(), n) = solver.eigenvalues();

    if (canonicalizeSigns)
    {
      detail::CanonicalizeEigenvectorColumnSigns(V);
    }
  }

  /** Eigenvectors as columns, sorted by increasing eigenvalue. */
  vnl_matrix<T> V;

  /** Eigenvalues in increasing order, stored as a diagonal matrix. */
  vnl_diag_matrix<T> D;

  /** Recover the i-th eigenvector (column i of V). */
  vnl_vector<T>
  get_eigenvector(int i) const
  {
    return V.get_column(i);
  }

  /** Recover the i-th eigenvalue. */
  T
  get_eigenvalue(int i) const
  {
    return D(i, i);
  }

  /** Least-squares nullvector: eigenvector of the smallest eigenvalue. */
  vnl_vector<T>
  nullvector() const
  {
    return V.get_column(0);
  }

  /** Reconstruct V D V^T (useful after modifying D). */
  vnl_matrix<T>
  recompose() const
  {
    return V * D * V.transpose();
  }
};

} // namespace itk

#endif // itkSymmetricEigenDecomposition_h
