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
#include "itkQRDecomposition.h"

// Exercise the deprecated VNL engine for the old-vs-new equivalence checks.
// The VNL symbol is unavailable under ITK_FUTURE_LEGACY_REMOVE.
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#  include "vnl/algo/vnl_qr.h"
#endif

#include <gtest/gtest.h>
#include <cmath>

namespace
{
template <typename T>
vnl_matrix<T>
MakeMatrix(unsigned int rows, unsigned int cols)
{
  vnl_matrix<T> A(rows, cols);
  for (unsigned int i = 0; i < rows; ++i)
    for (unsigned int j = 0; j < cols; ++j)
      A(i, j) = static_cast<T>(std::sin(0.6 * (i + 1) * (j + 3)) + 0.4 * (j + 1) - 0.2 * (i + 1));
  return A;
}
} // namespace


// Q R reconstructs A.
TEST(QRDecomposition, ReconstructsMatrix)
{
  const vnl_matrix<double>           A = MakeMatrix<double>(5, 5);
  const itk::QRDecomposition<double> qr(A);
  const vnl_matrix<double>           reconstructed = qr.GetQ() * qr.GetR();
  EXPECT_LT((reconstructed - A).fro_norm() / A.fro_norm(), 1e-12);
}


// Q is orthonormal: Q^T Q == I.
TEST(QRDecomposition, OrthonormalQ)
{
  const vnl_matrix<double>           A = MakeMatrix<double>(6, 4);
  const itk::QRDecomposition<double> qr(A);
  vnl_matrix<double>                 ident(6, 6);
  ident.set_identity();
  EXPECT_LT((qr.GetQ().transpose() * qr.GetQ() - ident).fro_norm(), 1e-12);
}


// A x = b solved; residual tiny.
TEST(QRDecomposition, SolveResidual)
{
  const unsigned int       n = 5;
  const vnl_matrix<double> A = MakeMatrix<double>(n, n);
  vnl_vector<double>       b(n);
  for (unsigned int i = 0; i < n; ++i)
    b[i] = static_cast<double>(i) - 1.5;

  const itk::QRDecomposition<double> qr(A);
  const vnl_vector<double>           x = qr.Solve(b);
  EXPECT_LT((A * x - b).two_norm() / b.two_norm(), 1e-10);
}


// Multi-column RHS: each column solves against the one shared factorization.
TEST(QRDecomposition, SolveMatrixRHS)
{
  const unsigned int       n = 5;
  const unsigned int       k = 3;
  const vnl_matrix<double> A = MakeMatrix<double>(n, n);
  vnl_matrix<double>       B(n, k);
  for (unsigned int i = 0; i < n; ++i)
    for (unsigned int j = 0; j < k; ++j)
      B(i, j) = std::cos(0.3 * (i + 1) * (j + 2));

  const itk::QRDecomposition<double> qr(A);
  const vnl_matrix<double>           X = qr.Solve(B);
  ASSERT_EQ(X.rows(), n);
  ASSERT_EQ(X.cols(), k);
  EXPECT_LT((A * X - B).fro_norm() / B.fro_norm(), 1e-10);
  for (unsigned int j = 0; j < k; ++j)
    EXPECT_LT((X.get_column(j) - qr.Solve(B.get_column(j))).two_norm(), 1e-12);
}


// Overdetermined (rows > cols) multi-column solve yields the least-squares
// solution, characterized by the normal equations A^T (A X - B) == 0.
TEST(QRDecomposition, SolveMatrixRHSOverdetermined)
{
  const unsigned int       m = 6;
  const unsigned int       n = 3;
  const unsigned int       k = 2;
  const vnl_matrix<double> A = MakeMatrix<double>(m, n);
  vnl_matrix<double>       B(m, k);
  for (unsigned int i = 0; i < m; ++i)
    for (unsigned int j = 0; j < k; ++j)
      B(i, j) = std::cos(0.35 * (i + 1) * (j + 2)) - 0.2 * (j + 1);

  const itk::QRDecomposition<double> qr(A);
  const vnl_matrix<double>           X = qr.Solve(B);
  ASSERT_EQ(X.rows(), n);
  ASSERT_EQ(X.cols(), k);
  EXPECT_LT((A.transpose() * (A * X - B)).fro_norm() / (A.transpose() * B).fro_norm(), 1e-10);
  for (unsigned int j = 0; j < k; ++j)
    EXPECT_LT((X.get_column(j) - qr.Solve(B.get_column(j))).two_norm(), 1e-12);
}


#ifndef ITK_FUTURE_LEGACY_REMOVE
// itk:: solve and determinant agree with the (sign-stable) legacy vnl_qr.
TEST(QRDecomposition, EquivalentToVnlQR)
{
  const unsigned int       n = 6;
  const vnl_matrix<double> A = MakeMatrix<double>(n, n);
  vnl_vector<double>       b(n);
  for (unsigned int i = 0; i < n; ++i)
    b[i] = std::cos(0.5 * (i + 1));

  const itk::QRDecomposition<double> qrItk(A);
  vnl_qr<double>                     qrVnl(A);

  EXPECT_LT((qrItk.Solve(b) - qrVnl.solve(b)).two_norm() / qrVnl.solve(b).two_norm(), 1e-10);
  EXPECT_NEAR(qrItk.GetDeterminant(), qrVnl.determinant(), 1e-9 * std::abs(qrVnl.determinant()) + 1e-12);
}


// Multi-column solve matches legacy vnl_qr column-for-column. This is the exact
// operation itkLandmarkBasedTransformInitializer relies on (square Q, matrix C).
TEST(QRDecomposition, MatrixRHSEquivalentToVnlQR)
{
  for (const unsigned int n : { 3u, 4u, 6u })
  {
    const unsigned int       k = n - 1;
    const vnl_matrix<double> A = MakeMatrix<double>(n, n);
    vnl_matrix<double>       B(n, k);
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = 0; j < k; ++j)
        B(i, j) = std::cos(0.4 * (i + 1) * (j + 2)) - 0.3 * (j + 1);

    const vnl_matrix<double> xItk = itk::QRDecomposition<double>(A).Solve(B);
    const vnl_matrix<double> xVnl = vnl_qr<double>(A).solve(B);
    EXPECT_LT((xItk - xVnl).fro_norm() / xVnl.fro_norm(), 1e-10);
  }
}
#endif


// Single-precision path reconstructs.
TEST(QRDecomposition, FloatReconstructs)
{
  const vnl_matrix<float>           A = MakeMatrix<float>(4, 4);
  const itk::QRDecomposition<float> qr(A);
  EXPECT_LT((qr.GetQ() * qr.GetR() - A).fro_norm() / A.fro_norm(), 1e-5f);
}


// Solve rejects an underdetermined system (rows < cols) instead of reading out of bounds.
TEST(QRDecomposition, SolveRejectsUnderdetermined)
{
  const vnl_matrix<double>           A = MakeMatrix<double>(3, 5);
  const itk::QRDecomposition<double> qr(A);
  const vnl_vector<double>           b(3, 1.0);
  EXPECT_THROW(qr.Solve(b), itk::ExceptionObject);
}
