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

// Exercises the vnl_cholesky engine end-to-end (inverse, solve, determinant,
// triangular factors, condition estimate), adapted from vxl
// core/vnl/algo/tests/test_cholesky.cxx so the coverage runs in ITK CI and
// guards any future change of the underlying Cholesky engine.

#include "itkBridgeCholeskySolve.h"

// The deprecated VNL engine under test is unavailable under ITK_FUTURE_LEGACY_REMOVE.
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#  include "vnl/algo/vnl_cholesky.h"
#  include "vnl/vnl_inverse.h"
#  include "vnl/vnl_random.h"

#  include <gtest/gtest.h>
#  include <cmath>
#  include <limits>

namespace
{
// Deterministic symmetric positive-definite matrix A = R R^T.
vnl_matrix<double>
MakeSPD(unsigned int n, unsigned int seed)
{
  vnl_random         rng(seed);
  vnl_matrix<double> R(n, n);
  for (unsigned int i = 0; i < n; ++i)
  {
    for (unsigned int j = 0; j < n; ++j)
    {
      R(i, j) = rng.drand32(-1.0, 1.0);
    }
  }
  return R * R.transpose();
}
} // namespace


// Scavenged from vxl test_cholesky: cholesky inverse matches the direct inverse.
TEST(VnlCholeskyEngine, InverseMatchesDirectInverse)
{
  const vnl_matrix<double> A = MakeSPD(3, 1000);
  const vnl_cholesky       chol(A);
  EXPECT_NEAR((chol.inverse() - vnl_inverse(A)).fro_norm(), 0.0, 1e-10);
}


// Scavenged from vxl test_cholesky: inverse is a two-sided identity, in both
// the default and estimate_condition modes.
TEST(VnlCholeskyEngine, InverseIsTwoSidedIdentity)
{
  const vnl_matrix<double> A = MakeSPD(3, 1000);
  vnl_matrix<double>       identity(3, 3);
  identity.set_identity();

  {
    const vnl_cholesky chol(A);
    EXPECT_NEAR((chol.inverse() * A - identity).fro_norm(), 0.0, 1e-10);
    EXPECT_NEAR((A * chol.inverse() - identity).fro_norm(), 0.0, 1e-10);
  }
  {
    const vnl_cholesky chol(A, vnl_cholesky::estimate_condition);
    EXPECT_NEAR((chol.inverse() * A - identity).fro_norm(), 0.0, 1e-10);
    EXPECT_NEAR((A * chol.inverse() - identity).fro_norm(), 0.0, 1e-10);
  }
}


// Scavenged from vxl test_cholesky: solve recovers a known solution; the
// out-parameter overload agrees with the returning overload.
TEST(VnlCholeskyEngine, SolveRecoversKnownSolution)
{
  const vnl_matrix<double> A = MakeSPD(3, 1000);
  vnl_random               rng(2000);
  vnl_vector<double>       x0(3);
  for (unsigned int i = 0; i < 3; ++i)
  {
    x0[i] = rng.drand32(-1.0, 1.0);
  }
  const vnl_vector<double> b = A * x0;

  const vnl_cholesky       chol(A);
  const vnl_vector<double> x = chol.solve(b);
  EXPECT_NEAR((x - x0).one_norm(), 0.0, 1e-6);

  vnl_vector<double> xOut(3);
  chol.solve(b, &xOut);
  EXPECT_NEAR((xOut - x).one_norm(), 0.0, 1e-14);
}


// determinant() equals the analytic determinant of L0 L0^T for a known lower
// factor L0: det = (prod diag(L0))^2.
TEST(VnlCholeskyEngine, DeterminantMatchesAnalytic)
{
  constexpr unsigned int n = 4;
  vnl_matrix<double>     L0(n, n, 0.0);
  double                 expected = 1.0;
  for (unsigned int i = 0; i < n; ++i)
  {
    L0(i, i) = static_cast<double>(i + 1);
    expected *= L0(i, i) * L0(i, i);
    for (unsigned int j = 0; j < i; ++j)
    {
      L0(i, j) = 0.25 * static_cast<double>(i + j + 1);
    }
  }
  const vnl_matrix<double> A = L0 * L0.transpose();

  const vnl_cholesky chol(A);
  EXPECT_NEAR(chol.determinant(), expected, 1e-9 * expected);
}


// lower_triangle()/upper_triangle() reconstruct A and are transposes.
TEST(VnlCholeskyEngine, TriangularFactorsReconstruct)
{
  const vnl_matrix<double> A = MakeSPD(5, 3000);
  const vnl_cholesky       chol(A);
  const vnl_matrix<double> L = chol.lower_triangle();
  const vnl_matrix<double> U = chol.upper_triangle();

  EXPECT_NEAR((L * L.transpose() - A).fro_norm() / A.fro_norm(), 0.0, 1e-12);
  EXPECT_NEAR((U - L.transpose()).fro_norm(), 0.0, 1e-14);
  for (unsigned int i = 0; i < 5; ++i)
  {
    for (unsigned int j = i + 1; j < 5; ++j)
    {
      EXPECT_EQ(L(i, j), 0.0);
    }
  }
}


// SPD input factors fully (rank_deficiency 0) with a usable condition estimate;
// an indefinite input reports non-positive-definiteness.
TEST(VnlCholeskyEngine, RankDeficiencyAndConditionEstimate)
{
  const vnl_matrix<double> A = MakeSPD(4, 4000);
  const vnl_cholesky       chol(A, vnl_cholesky::estimate_condition);
  EXPECT_EQ(chol.rank_deficiency(), 0);
  EXPECT_GT(chol.rcond(), std::sqrt(std::numeric_limits<double>::epsilon()));
  EXPECT_LE(chol.rcond(), 1.0);

  vnl_matrix<double> indefinite(2, 2);
  indefinite(0, 0) = 1.0;
  indefinite(0, 1) = 2.0;
  indefinite(1, 0) = 2.0;
  indefinite(1, 1) = 1.0; // eigenvalues 3, -1
  const vnl_cholesky badChol(indefinite, vnl_cholesky::quiet);
  EXPECT_GT(badChol.rank_deficiency(), 0);
}


// The engine agrees with the Eigen-backed replacement API it is deprecated for.
TEST(VnlCholeskyEngine, EquivalentToItkCholeskySolve)
{
  const vnl_matrix<double> A = MakeSPD(6, 5000);
  vnl_vector<double>       b(6);
  for (unsigned int i = 0; i < 6; ++i)
  {
    b[i] = std::cos(0.5 * (i + 1));
  }

  const vnl_cholesky       chol(A, vnl_cholesky::quiet);
  const vnl_vector<double> xVnl = chol.solve(b);
  const vnl_vector<double> xItk = itk::bridge::Math::SolveSymmetricPositiveDefinite(A, b);
  EXPECT_LT((xItk - xVnl).two_norm() / xVnl.two_norm(), 1e-10);
}

#endif // ITK_FUTURE_LEGACY_REMOVE
