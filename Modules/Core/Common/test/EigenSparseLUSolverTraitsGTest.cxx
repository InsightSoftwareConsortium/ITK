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

#include "EigenSparseLUSolverTraits.h"
#include "itkGTest.h"
#include "itkMath.h" // itk::Math::Absolute

#include <iostream>
#include <cstdlib>

template <class TVector>
bool
VectorsEquals(const TVector & v1, const TVector & v2, const typename TVector::Scalar & tolerance)
{
  if (v1.size() != v2.size())
  {
    std::cerr << "Error: v1.size() != v2.size()" << std::endl;
    return false;
  }

  for (unsigned int i = 0; i < v1.size(); ++i)
  {
    if (itk::Math::Absolute(v1(i) - v2(i)) > tolerance)
    {
      std::cerr << "Error: itk::Math::Absolute( v1(" << i << ") - v2(" << i << ") ) > " << tolerance << std::endl;
      return false;
    }
  }

  return true;
}

namespace
{
int
DoEigenSparseLUSolverTraitsTest(int, char *[])
{
  using CoordinateType = double;
  using SolverTraits = EigenSparseLUSolverTraits<CoordinateType>;
  using MatrixType = SolverTraits::MatrixType;
  using VectorType = SolverTraits::VectorType;

  constexpr unsigned int N{ 3 };
  VectorType             Bx = SolverTraits::InitializeVector(N);
  Bx.fill(0.);
  Bx[0] = 2.1;

  VectorType By = SolverTraits::InitializeVector(N);
  By.fill(0.);
  By[1] = 1.1;
  By[2] = -3.;

  VectorType Bz = SolverTraits::InitializeVector(N);
  Bz.fill(0.);
  Bz[0] = 19.4;
  Bz[1] = -4.3;

  MatrixType A = SolverTraits::InitializeSparseMatrix(N, N);
  SolverTraits::FillMatrix(A, 0, 0, 2);
  SolverTraits::FillMatrix(A, 0, 1, -1);
  SolverTraits::FillMatrix(A, 1, 0, -1);
  SolverTraits::FillMatrix(A, 1, 1, 2);
  SolverTraits::FillMatrix(A, 1, 2, -1);
  SolverTraits::FillMatrix(A, 2, 1, -1);
  SolverTraits::FillMatrix(A, 2, 2, 2);

  constexpr CoordinateType tolerance{ 1e-9 };

  VectorType Xexpected(N);
  Xexpected(0) = 1.575;
  Xexpected(1) = 1.05;
  Xexpected(2) = 0.525;

  VectorType Yexpected(N);
  Yexpected(0) = -0.2;
  Yexpected(1) = -0.4;
  Yexpected(2) = -1.7;

  VectorType Zexpected(N);
  Zexpected(0) = 12.4;
  Zexpected(1) = 5.4;
  Zexpected(2) = 2.7;

  /**
   * Test 1: Check the result of A * X = Bx
   */
  {
    VectorType X = SolverTraits::InitializeVector(N);
    SolverTraits::Solve(A, Bx, X);
    if (!VectorsEquals(X, Xexpected, tolerance))
    {
      return EXIT_FAILURE;
    }
  }

  /**
   * Test 2: Check the result of A * X = Bx, A * Y = By
   */
  {
    VectorType X = SolverTraits::InitializeVector(N);
    VectorType Y = SolverTraits::InitializeVector(N);
    SolverTraits::Solve(A, Bx, By, X, Y);
    if (!VectorsEquals(X, Xexpected, tolerance) || !VectorsEquals(Y, Yexpected, tolerance))
    {
      return EXIT_FAILURE;
    }
  }

  /**
   * Test 3: Check the result of A * X = Bx, A * Y = By, A * Z = Bz
   */
  {
    VectorType X = SolverTraits::InitializeVector(N);
    VectorType Y = SolverTraits::InitializeVector(N);
    VectorType Z = SolverTraits::InitializeVector(N);
    SolverTraits::Solve(A, Bx, By, Bz, X, Y, Z);
    if (!VectorsEquals(X, Xexpected, tolerance) || !VectorsEquals(Y, Yexpected, tolerance) ||
        !VectorsEquals(Z, Zexpected, tolerance))
    {
      return EXIT_FAILURE;
    }
  }

  /**
   * Test 4: Check the result of A * X = Bx (reuse the decomposed matrix for multiple back-substitutions)
   */
  {
    VectorType X = SolverTraits::InitializeVector(N);
    A.makeCompressed();
    SolverTraits::SolverType solver(A);

    // First back-substitution
    SolverTraits::Solve(solver, Bx, X);
    if (!VectorsEquals(X, Xexpected, tolerance))
    {
      return EXIT_FAILURE;
    }

    // Second back-substitution (reusing the already factored matrix)
    SolverTraits::Solve(solver, Bx, X);
    if (!VectorsEquals(X, Xexpected, tolerance))
    {
      return EXIT_FAILURE;
    }
  }

  /**
   * Test 5: Check the result of A * X = Bx, A * Y = By (reuse the decomposed matrix for multiple back-substitutions)
   */
  {
    VectorType X = SolverTraits::InitializeVector(N);
    VectorType Y = SolverTraits::InitializeVector(N);
    A.makeCompressed();
    SolverTraits::SolverType solver(A);

    // First back-substitution
    SolverTraits::Solve(solver, Bx, X);
    SolverTraits::Solve(solver, By, Y);
    if (!VectorsEquals(X, Xexpected, tolerance) || !VectorsEquals(Y, Yexpected, tolerance))
    {
      return EXIT_FAILURE;
    }

    // Second back-substitution (reusing the already factored matrix)
    SolverTraits::Solve(solver, Bx, X);
    SolverTraits::Solve(solver, By, Y);
    if (!VectorsEquals(X, Xexpected, tolerance) || !VectorsEquals(Y, Yexpected, tolerance))
    {
      return EXIT_FAILURE;
    }
  }

  /**
   * Test 6: Check the result of A * X = Bx, A * Y = By, A * Z = Bz (reuse the decomposed matrix for multiple
   * back-substitutions)
   */
  {
    VectorType X = SolverTraits::InitializeVector(N);
    VectorType Y = SolverTraits::InitializeVector(N);
    VectorType Z = SolverTraits::InitializeVector(N);
    A.makeCompressed();
    SolverTraits::SolverType solver(A);

    // First back-substitution
    SolverTraits::Solve(solver, Bx, By, Bz, X, Y, Z);
    if (!VectorsEquals(X, Xexpected, tolerance) || !VectorsEquals(Y, Yexpected, tolerance) ||
        !VectorsEquals(Z, Zexpected, tolerance))
    {
      return EXIT_FAILURE;
    }

    // Second back-substitution (reusing the already factored matrix)
    SolverTraits::Solve(solver, Bx, By, Bz, X, Y, Z);
    if (!VectorsEquals(X, Xexpected, tolerance) || !VectorsEquals(Y, Yexpected, tolerance) ||
        !VectorsEquals(Z, Zexpected, tolerance))
    {
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
} // namespace


TEST(EigenSparseLUSolverTraits, ConvertedLegacyTest) { EXPECT_EQ(0, DoEigenSparseLUSolverTraitsTest(0, nullptr)); }

// The following cases mirror vxl core/vnl/algo/tests/test_sparse_lu.cxx for the
// functionality reachable through Eigen::SparseLU. vnl_sparse_lu::rcond() and
// max_error_bound() have no Eigen::SparseLU equivalent and are not ported.

namespace
{
using SolverTraits = EigenSparseLUSolverTraits<double>;
using MatrixType = SolverTraits::MatrixType;
using VectorType = SolverTraits::VectorType;
using SolverType = SolverTraits::SolverType;
} // namespace

// mat0 of Kenneth S. Kundert's Sparse 1.3a release
TEST(EigenSparseLUSolverTraits, Mat0SolveDeterminantTranspose)
{
  MatrixType A = SolverTraits::InitializeSparseMatrix(4, 4);
  SolverTraits::FillMatrix(A, 0, 0, 2.0);
  SolverTraits::FillMatrix(A, 0, 1, -1.0);
  SolverTraits::FillMatrix(A, 1, 0, -1.0);
  SolverTraits::FillMatrix(A, 1, 1, 3.0);
  SolverTraits::FillMatrix(A, 1, 2, -1.0);
  SolverTraits::FillMatrix(A, 2, 1, -1.0);
  SolverTraits::FillMatrix(A, 2, 2, 3.0);
  SolverTraits::FillMatrix(A, 2, 3, -1.0);
  SolverTraits::FillMatrix(A, 3, 2, -1.0);
  SolverTraits::FillMatrix(A, 3, 3, 3.0);

  VectorType b = SolverTraits::InitializeVector(4);
  b.fill(0.0);
  b[0] = 34.0;

  VectorType x = SolverTraits::InitializeVector(4);
  EXPECT_TRUE(SolverTraits::Solve(A, b, x));
  EXPECT_NEAR(x[0], 21.0, 1.e-3);

  A.makeCompressed();
  SolverType solver(A);
  ASSERT_EQ(solver.info(), Eigen::Success);
  EXPECT_NEAR(solver.determinant(), 34.0, 1.e-3);

  // vnl_sparse_lu::solve_transpose equivalent
  const VectorType xt = solver.transpose().solve(b);
  EXPECT_NEAR(xt[2], 3.0, 1.e-3);
}

// mat5 of sparse test data: a permutation matrix
TEST(EigenSparseLUSolverTraits, PermutationMatrixSolve)
{
  MatrixType A = SolverTraits::InitializeSparseMatrix(3);
  SolverTraits::FillMatrix(A, 0, 1, 1.0);
  SolverTraits::FillMatrix(A, 1, 2, 1.0);
  SolverTraits::FillMatrix(A, 2, 0, 1.0);

  VectorType b = SolverTraits::InitializeVector(3);
  b[0] = 2.0;
  b[1] = 3.0;
  b[2] = 1.0;

  VectorType x = SolverTraits::InitializeVector(3);
  EXPECT_TRUE(SolverTraits::Solve(A, b, x));
  EXPECT_NEAR(x[2], 3.0, 1.e-3);
}

// matrix derived from a Poisson birth-death queue (near-singular, s = -0.01)
TEST(EigenSparseLUSolverTraits, BirthDeathQueueSolve)
{
  constexpr double s = -0.01;
  constexpr double l = 0.5;
  constexpr double m = 0.5;

  MatrixType S = SolverTraits::InitializeSparseMatrix(6);
  SolverTraits::FillMatrix(S, 0, 0, s + l);
  SolverTraits::FillMatrix(S, 0, 1, -l);
  SolverTraits::FillMatrix(S, 1, 0, -m);
  SolverTraits::FillMatrix(S, 1, 1, s + l + m);
  SolverTraits::FillMatrix(S, 1, 2, -l);
  SolverTraits::FillMatrix(S, 2, 1, -m);
  SolverTraits::FillMatrix(S, 2, 2, s + l + m);
  SolverTraits::FillMatrix(S, 3, 3, s + l + m);
  SolverTraits::FillMatrix(S, 3, 4, -l);
  SolverTraits::FillMatrix(S, 4, 3, -m);
  SolverTraits::FillMatrix(S, 4, 4, s + l + m);
  SolverTraits::FillMatrix(S, 4, 5, -l);
  SolverTraits::FillMatrix(S, 5, 4, -m);
  SolverTraits::FillMatrix(S, 5, 5, m + s);

  VectorType b = SolverTraits::InitializeVector(6);
  b.fill(0.0);
  b[2] = l;
  b[3] = m;

  VectorType x = SolverTraits::InitializeVector(6);
  EXPECT_TRUE(SolverTraits::Solve(S, b, x));
  EXPECT_NEAR(x[2], 1.06622, 1.e-4);
}
