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
#include "itkBridgeMathLDLT.h"

// Exercise the deprecated VNL engines as cross-check oracles.
// They are unavailable under ITK_FUTURE_LEGACY_REMOVE.
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#  include "vnl/algo/vnl_cholesky.h"
#  include "vnl/algo/vnl_matrix_inverse.h"
#endif

#include <gtest/gtest.h>
#include <cmath>

namespace
{
// Symmetric positive-definite matrix G^T G + shift*I, as an itk::Array2D.
template <typename T>
itk::Array2D<T>
MakeSPD(unsigned int n, T shift)
{
  vnl_matrix<T> G(n + 2, n);
  for (unsigned int i = 0; i < n + 2; ++i)
  {
    for (unsigned int j = 0; j < n; ++j)
    {
      G(i, j) = static_cast<T>(std::sin(0.7 * i + 1.3 * j));
    }
  }
  vnl_matrix<T> A = G.transpose() * G;
  for (unsigned int i = 0; i < n; ++i)
  {
    A(i, i) += shift;
  }
  return itk::Array2D<T>(A);
}
} // namespace

// The capability macro must be defined by the header.
#ifndef ITK_BRIDGE_MATH_HAS_SOLVE_SYMMETRIC
#  error "itkBridgeMathLDLT.h must define ITK_BRIDGE_MATH_HAS_SOLVE_SYMMETRIC"
#endif

TEST(BridgeMathLDLT, DynamicMatchesVnlCholeskyOnSPD)
{
  for (unsigned int n : { 1u, 2u, 3u, 6u, 12u, 20u })
  {
    const itk::Array2D<double> A = MakeSPD<double>(n, 0.5);
    itk::Array<double>         b(n);
    for (unsigned int i = 0; i < n; ++i)
    {
      b[i] = std::cos(0.3 * i + 1.0);
    }

    const itk::Array<double> x = itk::bridge::Math::SolveSymmetric(A, b);
    ASSERT_EQ(x.size(), n);
#ifndef ITK_FUTURE_LEGACY_REMOVE
    // Reference: itk::Array2D / itk::Array upcast to their vnl bases.
    const vnl_vector<double> ref = vnl_cholesky(A).solve(b);
    for (unsigned int i = 0; i < n; ++i)
    {
      EXPECT_NEAR(x[i], ref[i], 1e-10) << "n=" << n << " i=" << i;
    }
#endif
    // Residual A x - b is near zero (vnl ops via the base classes).
    const vnl_vector<double> r = A * x - b;
    EXPECT_LT(r.inf_norm(), 1e-9) << "n=" << n;
  }
}

TEST(BridgeMathLDLT, FixedMatchesDynamic)
{
  constexpr unsigned int     VDim = 4;
  const itk::Array2D<double> Ad = MakeSPD<double>(VDim, 0.3);

  itk::Matrix<double, VDim, VDim> Af;
  itk::Vector<double, VDim>       bf;
  itk::Array<double>              bd(VDim);
  for (unsigned int i = 0; i < VDim; ++i)
  {
    for (unsigned int j = 0; j < VDim; ++j)
    {
      Af(i, j) = Ad(i, j);
    }
    bf[i] = bd[i] = std::cos(0.3 * i + 1.0);
  }

  const itk::Vector<double, VDim> xf = itk::bridge::Math::SolveSymmetric(Af, bf);
  const itk::Array<double>        xd = itk::bridge::Math::SolveSymmetric(Ad, bd);
  for (unsigned int i = 0; i < VDim; ++i)
  {
    EXPECT_NEAR(xf[i], xd[i], 1e-12) << "i=" << i;
  }
}

// LDLT solves indefinite symmetric systems where a plain Cholesky (SPD-only)
// would fail -- the key robustness property motivating the JLF adoption.
TEST(BridgeMathLDLT, HandlesIndefiniteSymmetric)
{
  constexpr unsigned int    n = 3;
  itk::Matrix<double, n, n> A;
  A.Fill(0.0);
  A(0, 0) = 2.0;
  A(1, 1) = -1.0; // negative eigenvalue -> indefinite
  A(2, 2) = 3.0;
  A(0, 1) = A(1, 0) = 0.5;
  itk::Vector<double, n> b;
  b[0] = 1.0;
  b[1] = 2.0;
  b[2] = -1.0;

  const itk::Vector<double, n> x = itk::bridge::Math::SolveSymmetric(A, b);
  const itk::Vector<double, n> r = A * x - b;
  EXPECT_LT(r.GetNorm(), 1e-10);
}

// The vnl convenience overload (for consumers still holding vnl types, e.g. the
// ANTs joint-label-fusion MxBar) must agree with the ITK-typed path.
TEST(BridgeMathLDLT, VnlConvenienceMatchesItkTyped)
{
  constexpr unsigned int     n = 8;
  const itk::Array2D<double> A = MakeSPD<double>(n, 0.5);
  itk::Array<double>         b(n);
  for (unsigned int i = 0; i < n; ++i)
  {
    b[i] = std::cos(0.3 * i + 1.0);
  }

  // vnl overload: A / b upcast to their vnl bases.
  const vnl_matrix<double> & Avnl = A;
  const vnl_vector<double> & bvnl = b;
  const vnl_vector<double>   xvnl = itk::bridge::Math::SolveSymmetric(Avnl, bvnl);
  const itk::Array<double>   xitk = itk::bridge::Math::SolveSymmetric(A, b);

  ASSERT_EQ(xvnl.size(), n);
  for (unsigned int i = 0; i < n; ++i)
  {
    EXPECT_NEAR(xvnl[i], xitk[i], 1e-12) << "i=" << i;
  }
}

TEST(BridgeMathLDLT, InverseSymmetricMatchesVnl)
{
  for (unsigned int n : { 1u, 3u, 6u, 12u })
  {
    const itk::Array2D<double> A = MakeSPD<double>(n, 0.5);
    const itk::Array2D<double> inv = itk::bridge::Math::InverseSymmetric(A);
    ASSERT_EQ(inv.rows(), n);
#ifndef ITK_FUTURE_LEGACY_REMOVE
    const vnl_matrix<double> ref = vnl_matrix_inverse<double>(A).inverse();
    for (unsigned int i = 0; i < n; ++i)
    {
      for (unsigned int j = 0; j < n; ++j)
      {
        EXPECT_NEAR(inv(i, j), ref(i, j), 1e-9) << "n=" << n << " (" << i << "," << j << ")";
      }
    }
#endif
    // A * inv == I
    const vnl_matrix<double> prod = A * inv;
    for (unsigned int i = 0; i < n; ++i)
    {
      for (unsigned int j = 0; j < n; ++j)
      {
        EXPECT_NEAR(prod(i, j), (i == j ? 1.0 : 0.0), 1e-9);
      }
    }
  }
}

TEST(BridgeMathLDLT, InverseSymmetricThrowsOnSingular)
{
  // Rank-1 symmetric matrix [[1,2],[2,4]] is exactly singular.
  itk::Array2D<double> A(2, 2);
  A(0, 0) = 1.0;
  A(0, 1) = A(1, 0) = 2.0;
  A(1, 1) = 4.0;
  EXPECT_THROW(itk::bridge::Math::InverseSymmetric(A), itk::ExceptionObject);

  const vnl_matrix<double> & Avnl = A;
  EXPECT_THROW(itk::bridge::Math::InverseSymmetric(Avnl), itk::ExceptionObject);

  itk::Array2D<double> zero(3, 3);
  zero.fill(0.0);
  EXPECT_THROW(itk::bridge::Math::InverseSymmetric(zero), itk::ExceptionObject);
}

TEST(BridgeMathLDLT, MatrixRhsSolveMatchesColumnwise)
{
  constexpr unsigned int     n = 6;
  const itk::Array2D<double> A = MakeSPD<double>(n, 0.4);
  itk::Array2D<double>       B(n, 3);
  for (unsigned int i = 0; i < n; ++i)
  {
    for (unsigned int j = 0; j < 3; ++j)
    {
      B(i, j) = std::cos(0.3 * i + 0.7 * j);
    }
  }

  const itk::Array2D<double> X = itk::bridge::Math::SolveSymmetric(A, B);
  // A X == B
  const vnl_matrix<double> prod = A * X;
  for (unsigned int i = 0; i < n; ++i)
  {
    for (unsigned int j = 0; j < 3; ++j)
    {
      EXPECT_NEAR(prod(i, j), B(i, j), 1e-9) << "(" << i << "," << j << ")";
    }
  }
}

TEST(BridgeMathLDLT, FloatPrecision)
{
  const itk::Array2D<float> A = MakeSPD<float>(5, 0.5f);
  itk::Array<float>         b(5);
  for (unsigned int i = 0; i < 5; ++i)
  {
    b[i] = static_cast<float>(std::cos(0.3 * i));
  }
  const itk::Array<float> x = itk::bridge::Math::SolveSymmetric(A, b);
  const vnl_vector<float> r = A * x - b;
  EXPECT_LT(r.inf_norm(), 1e-4f);
}
