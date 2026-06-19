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
#include "itkGeneralizedEigenDecomposition.h"
#include "itkConfigure.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#  include "vnl/algo/vnl_generalized_eigensystem.h"
#endif
#include <gtest/gtest.h>
#include <limits>
#include <string>

namespace
{

// Frobenius residual of the generalized identity A V == B V D.
template <typename TReal>
TReal
GeneralizedResidual(const vnl_matrix<TReal> &                         A,
                    const vnl_matrix<TReal> &                         B,
                    const itk::GeneralizedEigenDecomposition<TReal> & ge)
{
  const vnl_matrix<TReal> & V = ge.GetEigenvectors();
  vnl_matrix<TReal>         D(V.rows(), V.cols(), TReal{ 0 });
  for (unsigned int i = 0; i < V.cols(); ++i)
  {
    D(i, i) = ge.GetEigenvalues()[i];
  }
  return (A * V - B * V * D).fro_norm();
}

} // namespace

// Scavenged from VXL test_generalized_eigensystem: A x = lambda B x with A
// symmetric, B symmetric positive-definite. (Roles match the vnl ctor's
// (A, B) order; the VXL test's residual is C V - S V D for gev(C, S).)
TEST(GeneralizedEigenDecomposition, SymmetricDefinitePencil)
{
  const double Adata[36] = {
    30.0000, -3.4273, 13.9254, 13.7049, -2.4446, 20.2380, -3.4273, 13.7049, -2.4446, 1.3659,  3.6702,  -0.2282,
    13.9254, -2.4446, 20.2380, 3.6702,  -0.2282, 28.6779, 13.7049, 1.3659,  3.6702,  12.5273, -1.6045, 3.9419,
    -2.4446, 3.6702,  -0.2282, -1.6045, 3.9419,  2.5821,  20.2380, -0.2282, 28.6779, 3.9419,  2.5821,  44.0636,
  };
  const vnl_matrix<double> A(Adata, 6, 6);

  // Symmetric positive-definite B (diagonally dominant).
  vnl_matrix<double> B(6, 6, 0.0);
  for (unsigned int i = 0; i < 6; ++i)
  {
    B(i, i) = 10.0 + i;
  }
  B(0, 1) = B(1, 0) = 1.0;
  B(2, 3) = B(3, 2) = -2.0;

  const itk::GeneralizedEigenDecomposition<double> ge(A, B);

  // Eigenvalues are real and ascending.
  for (unsigned int i = 1; i < 6; ++i)
  {
    EXPECT_LE(ge.GetEigenvalues()[i - 1], ge.GetEigenvalues()[i] + 1e-12);
  }
  EXPECT_LT(GeneralizedResidual(A, B, ge), 1e-11);
}

// B = I reduces to the ordinary symmetric eigenproblem; this mirrors how
// ImagePCAShapeModelEstimator invokes the solver.
TEST(GeneralizedEigenDecomposition, IdentityPencilMatchesStandard)
{
  vnl_matrix<double> A(3, 3, 0.0);
  A(0, 0) = 2.0;
  A(1, 1) = 5.0;
  A(2, 2) = 11.0;
  A(0, 1) = A(1, 0) = 1.0;
  A(1, 2) = A(2, 1) = -1.0;

  vnl_matrix<double> I(3, 3, 0.0);
  for (unsigned int i = 0; i < 3; ++i)
  {
    I(i, i) = 1.0;
  }

  const itk::GeneralizedEigenDecomposition<double> ge(A, I);
  EXPECT_LT(GeneralizedResidual(A, I, ge), 1e-12);

  // For B = I the eigenvectors are orthonormal: V^T V == I.
  const vnl_matrix<double> vtv = ge.GetEigenvectors().transpose() * ge.GetEigenvectors();
  for (unsigned int r = 0; r < 3; ++r)
  {
    for (unsigned int c = 0; c < 3; ++c)
    {
      EXPECT_NEAR(vtv(r, c), (r == c) ? 1.0 : 0.0, 1e-12);
    }
  }
}

TEST(GeneralizedEigenDecomposition, FloatInstantiation)
{
  vnl_matrix<float> A(2, 2);
  A(0, 0) = 3.0f;
  A(1, 1) = 7.0f;
  A(0, 1) = A(1, 0) = 0.5f;
  vnl_matrix<float> B(2, 2, 0.0f);
  B(0, 0) = 2.0f;
  B(1, 1) = 4.0f;

  const itk::GeneralizedEigenDecomposition<float> ge(A, B);
  EXPECT_LT(GeneralizedResidual(A, B, ge), 1e-5f);
}

// Non-finite input leaves the Eigen solver in a non-Success state; the wrapper
// turns that into an exception so callers do not silently consume a garbage
// decomposition.
TEST(GeneralizedEigenDecomposition, NonFiniteInputThrows)
{
  const double       nan = std::numeric_limits<double>::quiet_NaN();
  vnl_matrix<double> A(2, 2, 0.0);
  A(0, 0) = 1.0;
  A(1, 1) = 1.0;
  vnl_matrix<double> B(2, 2, 0.0);
  B(0, 0) = 1.0;
  B(1, 1) = 1.0;
  B(0, 1) = B(1, 0) = nan;
  try
  {
    const itk::GeneralizedEigenDecomposition<double> ge(A, B);
    FAIL() << "expected an exception for non-finite input";
  }
  catch (const itk::ExceptionObject & e)
  {
    const std::string description = e.GetDescription();
    EXPECT_NE(description.find("GeneralizedSelfAdjointEigenSolver"), std::string::npos) << description;
    // The message names the decoded Eigen failure mode, not a bare status code.
    EXPECT_TRUE(description.find("NoConvergence") != std::string::npos ||
                description.find("NumericalIssue") != std::string::npos ||
                description.find("InvalidInput") != std::string::npos)
      << description;
  }
}

#ifndef ITK_FUTURE_LEGACY_REMOVE
// Equivalence: the Eigen-backed solver reproduces the eigenvalues of the
// deprecated netlib vnl_generalized_eigensystem it replaces (both ascending).
TEST(GeneralizedEigenDecomposition, MatchesDeprecatedVnlEngine)
{
  vnl_matrix<double> A(3, 3, 0.0);
  A(0, 0) = 2.0;
  A(1, 1) = 5.0;
  A(2, 2) = 11.0;
  A(0, 1) = A(1, 0) = 1.0;
  A(1, 2) = A(2, 1) = -1.0;
  vnl_matrix<double> B(3, 3, 0.0);
  B(0, 0) = 3.0;
  B(1, 1) = 2.0;
  B(2, 2) = 4.0;
  B(0, 1) = B(1, 0) = 0.5;

  const vnl_generalized_eigensystem                vnlEngine(A, B);
  const itk::GeneralizedEigenDecomposition<double> itkEngine(A, B);

  const vnl_vector<double>   vnlEigenvalues = vnlEngine.D.get_diagonal();
  const vnl_vector<double> & itkEigenvalues = itkEngine.GetEigenvalues();
  ASSERT_EQ(vnlEigenvalues.size(), itkEigenvalues.size());
  for (unsigned int i = 0; i < itkEigenvalues.size(); ++i)
  {
    EXPECT_NEAR(vnlEigenvalues[i], itkEigenvalues[i], 1e-10) << "eigenvalue " << i;
  }
}
#endif
