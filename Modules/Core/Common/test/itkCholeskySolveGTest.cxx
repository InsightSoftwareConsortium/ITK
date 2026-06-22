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

// First include the header file to be tested:
#include "itkCholeskySolve.h"

// Exercise the deprecated VNL engine for the old-vs-new equivalence checks.
// The VNL symbol is unavailable under ITK_FUTURE_LEGACY_REMOVE.
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#  include "vnl/algo/vnl_cholesky.h"
#endif

#include <gtest/gtest.h>
#include <cmath>

namespace
{
// Build a symmetric positive-definite matrix A = R R' + n*I from a fixed seed.
template <typename T>
vnl_matrix<T>
MakeSPD(unsigned int n)
{
  vnl_matrix<T> R(n, n);
  for (unsigned int i = 0; i < n; ++i)
    for (unsigned int j = 0; j < n; ++j)
      R(i, j) = static_cast<T>(std::sin(0.7 * (i + 1) * (j + 2)) + 0.3 * (i + 1));
  vnl_matrix<T> A = R * R.transpose();
  for (unsigned int i = 0; i < n; ++i)
    A(i, i) += static_cast<T>(n);
  return A;
}
} // namespace


// A x = b is recovered: residual ||A x - b|| is tiny.
TEST(CholeskySolve, SolveResidual)
{
  const unsigned int       n = 5;
  const vnl_matrix<double> A = MakeSPD<double>(n);
  vnl_vector<double>       b(n);
  for (unsigned int i = 0; i < n; ++i)
    b[i] = static_cast<double>(i) - 1.5;

  const vnl_vector<double> x = itk::Math::SolveSymmetricPositiveDefinite(A, b);
  const vnl_vector<double> residual = A * x - b;
  EXPECT_LT(residual.two_norm() / b.two_norm(), 1e-12);
}


// CholeskyLowerTriangle returns L with A == L L'.
TEST(CholeskySolve, LowerTriangleReconstructsMatrix)
{
  const unsigned int       n = 4;
  const vnl_matrix<double> A = MakeSPD<double>(n);
  const vnl_matrix<double> L = itk::Math::CholeskyLowerTriangle(A);

  // L is lower triangular.
  for (unsigned int i = 0; i < n; ++i)
    for (unsigned int j = i + 1; j < n; ++j)
      EXPECT_NEAR(L(i, j), 0.0, 1e-12);

  const vnl_matrix<double> reconstructed = L * L.transpose();
  EXPECT_LT((reconstructed - A).fro_norm() / A.fro_norm(), 1e-12);
}


#ifndef ITK_FUTURE_LEGACY_REMOVE
// The Eigen-backed itk:: solve agrees with the native vnl_cholesky engine.
TEST(CholeskySolve, EquivalentToVnlCholesky)
{
  const unsigned int       n = 6;
  const vnl_matrix<double> A = MakeSPD<double>(n);
  vnl_vector<double>       b(n);
  for (unsigned int i = 0; i < n; ++i)
    b[i] = std::cos(0.5 * (i + 1));

  const vnl_vector<double> xItk = itk::Math::SolveSymmetricPositiveDefinite(A, b);

  const vnl_cholesky       chol(A, vnl_cholesky::quiet);
  const vnl_vector<double> xVnl = chol.solve(b);

  EXPECT_LT((xItk - xVnl).two_norm() / xVnl.two_norm(), 1e-10);
}
#endif


// Single-precision path solves correctly.
TEST(CholeskySolve, FloatResidual)
{
  const unsigned int      n = 4;
  const vnl_matrix<float> A = MakeSPD<float>(n);
  vnl_vector<float>       b(n);
  for (unsigned int i = 0; i < n; ++i)
    b[i] = static_cast<float>(i) + 0.25f;

  const vnl_vector<float> x = itk::Math::SolveSymmetricPositiveDefinite(A, b);
  const vnl_vector<float> residual = A * x - b;
  EXPECT_LT(residual.two_norm() / b.two_norm(), 1e-4f);
}


// CholeskyLowerTriangle rejects a non-positive-definite matrix instead of returning a garbage factor.
TEST(CholeskySolve, LowerTriangleRejectsNonPositiveDefinite)
{
  vnl_matrix<double> A(2, 2);
  A(0, 0) = 1.0;
  A(0, 1) = 2.0;
  A(1, 0) = 2.0;
  A(1, 1) = 1.0; // symmetric but indefinite (eigenvalues 3, -1)
  EXPECT_THROW(itk::Math::CholeskyLowerTriangle(A), itk::ExceptionObject);
}
