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
#ifndef itkRealEigenDecomposition_h
#define itkRealEigenDecomposition_h

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include <complex>
#include "itkEigenDecompositionSolverInfo.h"
#include "itkMacro.h"
#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

namespace itk
{

/** \class RealEigenDecomposition
 * \brief Eigenvalues and eigenvectors of a general real matrix, backed by Eigen.
 *
 * Solves the eigenproblem of a general (non-symmetric) real matrix once in the
 * constructor via Eigen's EigenSolver; the spectrum is in general complex.
 * Results are stored as vnl containers of std::complex so no Eigen type appears
 * in the public interface. For a symmetric matrix prefer
 * SymmetricEigenDecomposition (real spectrum, faster).
 *
 * This is the supported Eigen-backed replacement for the now-deprecated
 * vnl_real_eigensystem (netlib EISPACK rg), which emits a deprecation warning
 * under ITK_LEGACY_REMOVE and is removed under ITK_FUTURE_LEGACY_REMOVE.
 *
 * \note A complex eigenvector is defined only up to an arbitrary unit-modulus
 * phase (not merely a sign), so no deterministic phase canonicalization is
 * applied here (unlike the real-vector SymmetricEigenDecomposition /
 * GeneralizedEigenDecomposition). Migrate to the identity A V == V D rather
 * than to specific eigenvector phases.
 *
 * \ingroup ITKCommon
 */
template <typename TReal>
class RealEigenDecomposition
{
public:
  using ComplexType = std::complex<TReal>;
  using ComplexMatrixType = vnl_matrix<ComplexType>;
  using ComplexVectorType = vnl_vector<ComplexType>;

  explicit RealEigenDecomposition(const vnl_matrix<TReal> & M)
  {
    using RowMajor = Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    const unsigned int n = M.rows();

    Eigen::Map<const RowMajor>                                                     mMap(M.data_block(), n, M.cols());
    const Eigen::EigenSolver<Eigen::Matrix<TReal, Eigen::Dynamic, Eigen::Dynamic>> solver(mMap);

    if (solver.info() != Eigen::Success)
    {
      itkGenericExceptionMacro(<< "RealEigenDecomposition: Eigen EigenSolver failed: "
                               << detail::EigenComputationInfoString(solver.info()) << '.');
    }

    using ComplexRowMajor = Eigen::Matrix<ComplexType, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
    m_Eigenvalues.set_size(n);
    Eigen::Map<Eigen::Matrix<ComplexType, Eigen::Dynamic, 1>>(m_Eigenvalues.data_block(), n) = solver.eigenvalues();
    m_Eigenvectors.set_size(n, n);
    Eigen::Map<ComplexRowMajor>(m_Eigenvectors.data_block(), n, n) = solver.eigenvectors();
  }

  /** Complex eigenvalues, one per column of M (unsorted, Eigen order). */
  const ComplexVectorType &
  GetEigenvalues() const
  {
    return m_Eigenvalues;
  }

  /** Complex eigenvectors as columns, aligned with GetEigenvalues(). */
  const ComplexMatrixType &
  GetEigenvectors() const
  {
    return m_Eigenvectors;
  }

private:
  ComplexVectorType m_Eigenvalues;
  ComplexMatrixType m_Eigenvectors;
};

} // namespace itk

#endif // itkRealEigenDecomposition_h
