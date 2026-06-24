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

// Equivalence gate for swapping the vnl_sparse_lu engine to Eigen::SparseLU:
// solving the same logical system through VNLSparseLUSolverTraits and
// SparseLUSolverTraits must yield the same solution. Both traits are
// direct LU solvers for the same A x = b, so agreement to tight tolerance is
// required before the Eigen algorithm may be placed behind the vnl_* API.

#include "itkConfigure.h" // defines ITK_FUTURE_LEGACY_REMOVE before the guard below
// The vnl_sparse_lu equivalence comparison is unavailable under
// ITK_FUTURE_LEGACY_REMOVE, where VNLSparseLUSolverTraits is removed.
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST // intentionally exercises the deprecated VNLSparseLUSolverTraits for equivalence
#  include "VNLSparseLUSolverTraits.h"
#  include "SparseLUSolverTraits.h"
#  include "itkGTest.h"

#  include <cmath>
#  include <vector>

namespace
{
struct Triplet
{
  unsigned int r, c;
  double       v;
};

// 2-D 5-point Laplacian on an n x n grid with a +shift diagonal so the matrix
// is nonsingular. N = n*n unknowns. Representative of QuadEdgeMesh
// parameterization Laplacians.
unsigned int
makeLaplacian2D(unsigned int n, double diagShift, std::vector<Triplet> & out)
{
  const unsigned int N = n * n;
  out.clear();
  auto idx = [n](unsigned int i, unsigned int j) { return i * n + j; };
  for (unsigned int i = 0; i < n; ++i)
  {
    for (unsigned int j = 0; j < n; ++j)
    {
      const unsigned int p = idx(i, j);
      double             diag = 4.0 + diagShift;
      if (i + 1 < n)
        out.push_back({ p, idx(i + 1, j), -1.0 });
      else
        diag -= 1.0;
      if (i > 0)
        out.push_back({ p, idx(i - 1, j), -1.0 });
      else
        diag -= 1.0;
      if (j + 1 < n)
        out.push_back({ p, idx(i, j + 1), -1.0 });
      else
        diag -= 1.0;
      if (j > 0)
        out.push_back({ p, idx(i, j - 1), -1.0 });
      else
        diag -= 1.0;
      out.push_back({ p, p, diag });
    }
  }
  return N;
}

template <typename Traits>
std::vector<double>
solveWith(const std::vector<Triplet> & trips, const std::vector<double> & rhs, unsigned int N)
{
  auto A = Traits::InitializeSparseMatrix(N, N);
  for (const auto & t : trips)
    Traits::AddToMatrix(A, t.r, t.c, t.v);
  auto b = Traits::InitializeVector(N);
  for (unsigned int i = 0; i < N; ++i)
    b[i] = rhs[i];
  auto x = Traits::InitializeVector(N);
  EXPECT_TRUE(Traits::Solve(A, b, x));
  return std::vector<double>(&x[0], &x[0] + N);
}

void
expectEquivalent(unsigned int n, double shift, double tol)
{
  std::vector<Triplet> trips;
  const unsigned int   N = makeLaplacian2D(n, shift, trips);
  std::vector<double>  rhs(N);
  for (unsigned int i = 0; i < N; ++i)
    rhs[i] = std::sin(0.013 * (i + 1)) + 0.25;

  const auto xVnl = solveWith<VNLSparseLUSolverTraits<double>>(trips, rhs, N);
  const auto xEig = solveWith<SparseLUSolverTraits<double>>(trips, rhs, N);

  ASSERT_EQ(xVnl.size(), xEig.size());
  // Relative agreement: an ill-conditioned A makes ||x|| huge for an arbitrary
  // RHS, so the two direct solvers are compared relative to the solution scale,
  // not by absolute difference.
  double maxDiff = 0.0;
  double scale = 1.0;
  for (unsigned int i = 0; i < N; ++i)
  {
    maxDiff = std::max(maxDiff, std::abs(xVnl[i] - xEig[i]));
    scale = std::max(scale, std::abs(xVnl[i]));
  }
  const double relDiff = maxDiff / scale;
  EXPECT_LE(relDiff, tol) << "VNL vs Eigen sparse-LU solutions diverge by " << relDiff << " relative (abs " << maxDiff
                          << ", scale " << scale << ", n=" << n << ", shift=" << shift << ')';
}
} // namespace

// Well-conditioned systems across sizes: the two direct solvers must agree to
// near machine precision.
TEST(VnlEigenSparseLUEquivalence, WellConditioned)
{
  expectEquivalent(8, 1e-3, 1e-9);
  expectEquivalent(16, 1e-3, 1e-9);
  expectEquivalent(32, 1e-3, 1e-9);
}

// Ill-conditioned system: tolerance is looser because both solvers amplify the
// conditioning, but they must still track each other closely.
TEST(VnlEigenSparseLUEquivalence, IllConditioned) { expectEquivalent(24, 1e-9, 1e-6); }
#endif
