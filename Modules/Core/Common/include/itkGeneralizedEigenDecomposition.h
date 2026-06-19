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
#ifndef itkGeneralizedEigenDecomposition_h
#define itkGeneralizedEigenDecomposition_h

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "itkEigenDecompositionSignConvention.h"
#include "itkEigenDecompositionSolverInfo.h"
#include "itkMacro.h"
#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

namespace itk
{

/** \class GeneralizedEigenDecomposition
 * \brief Symmetric-definite generalized eigenproblem A x = lambda B x, Eigen-backed.
 *
 * Solves A x = lambda B x with A symmetric and B symmetric positive-definite,
 * via Eigen's GeneralizedSelfAdjointEigenSolver. Eigenvalues are real and
 * returned in ascending order; eigenvectors are the B-orthonormal columns,
 * sign-canonicalized (each column's largest-magnitude entry positive) for
 * cross-platform reproducibility. Results are stored in vnl containers so no
 * Eigen type appears in the public interface.
 *
 * This is the supported Eigen-backed replacement for the now-deprecated
 * vnl_generalized_eigensystem (netlib EISPACK rsg), which emits a deprecation
 * warning under ITK_LEGACY_REMOVE and is removed under ITK_FUTURE_LEGACY_REMOVE.
 *
 * \ingroup ITKCommon
 */
template <typename TReal>
class GeneralizedEigenDecomposition
{
public:
  using MatrixType = vnl_matrix<TReal>;
  using VectorType = vnl_vector<TReal>;

  /** Solve A x = lambda B x for symmetric A and symmetric-positive-definite B.
   * With \a canonicalizeSigns (the default) each eigenvector column's
   * largest-magnitude entry is made positive for cross-platform reproducibility;
   * pass false to keep the solver's raw signs. */
  GeneralizedEigenDecomposition(const MatrixType & A, const MatrixType & B, bool canonicalizeSigns = true)
  {
    using RowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    using ColMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic>;
    using Vector = Eigen::Matrix<TReal, Eigen::Dynamic, 1>;
    const unsigned int n = A.rows();

    Eigen::Map<const RowMajor> aMap(A.data_block(), n, n);
    Eigen::Map<const RowMajor> bMap(B.data_block(), n, n);

    const Eigen::GeneralizedSelfAdjointEigenSolver<ColMajor> solver(aMap, bMap);

    if (solver.info() != Eigen::Success)
    {
      itkGenericExceptionMacro(<< "GeneralizedEigenDecomposition: Eigen GeneralizedSelfAdjointEigenSolver failed: "
                               << detail::EigenComputationInfoString(solver.info())
                               << "; A and B must be finite and B symmetric positive-definite.");
    }

    m_Eigenvalues.set_size(n);
    Eigen::Map<Vector>(m_Eigenvalues.data_block(), n) = solver.eigenvalues();
    m_Eigenvectors.set_size(n, n);
    Eigen::Map<RowMajor>(m_Eigenvectors.data_block(), n, n) = solver.eigenvectors();

    if (canonicalizeSigns)
    {
      detail::CanonicalizeEigenvectorColumnSigns(m_Eigenvectors);
    }
  }

  /** Real eigenvalues in ascending order. */
  const VectorType &
  GetEigenvalues() const
  {
    return m_Eigenvalues;
  }

  /** B-orthonormal eigenvectors as columns, aligned with GetEigenvalues(). */
  const MatrixType &
  GetEigenvectors() const
  {
    return m_Eigenvectors;
  }

private:
  VectorType m_Eigenvalues;
  MatrixType m_Eigenvectors;
};

} // namespace itk

#endif // itkGeneralizedEigenDecomposition_h
