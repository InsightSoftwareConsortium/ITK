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

// Exercises vnl_svd end-to-end (inversion, least-squares solve, rank/nullspace,
// recomposition) across float/double/complex, adapted from vxl
// core/vnl/algo/tests/test_svd.cxx so the coverage runs in ITK CI and guards
// any future change of the underlying SVD engine.

#include "itkConfigure.h"

// The deprecated engine under test is unavailable under ITK_FUTURE_LEGACY_REMOVE.
#ifndef ITK_FUTURE_LEGACY_REMOVE

#  define ITK_LEGACY_TEST
#  include "vnl/algo/vnl_svd.h"
#  include "vnl/vnl_random.h"

#  include <gtest/gtest.h>
#  include <complex>

namespace
{
// Deterministic random element in [-1,1]; complex fills real and imaginary.
template <typename T>
struct RandFill
{
  static T
  get(vnl_random & r)
  {
    return static_cast<T>(r.drand32(-1.0, 1.0));
  }
};
template <typename R>
struct RandFill<std::complex<R>>
{
  static std::complex<R>
  get(vnl_random & r)
  {
    return { static_cast<R>(r.drand32(-1.0, 1.0)), static_cast<R>(r.drand32(-1.0, 1.0)) };
  }
};

template <typename T>
void
fillRandom(vnl_matrix<T> & m, vnl_random & r)
{
  for (unsigned int i = 0; i < m.rows(); ++i)
    for (unsigned int j = 0; j < m.cols(); ++j)
      m(i, j) = RandFill<T>::get(r);
}

// H * inv(H) == I for a 5x5 Hilbert matrix (well-conditioned enough at n=5).
template <typename T>
void
hilbertInverse(double residualTol)
{
  using abs_t = typename vnl_numeric_traits<T>::abs_t;
  vnl_matrix<T> H(5, 5);
  for (int i = 0; i < 5; ++i)
    for (int j = 0; j < 5; ++j)
      H(i, j) = T(1) / T(abs_t(i + j + 1));

  const vnl_svd<T> svd(H);
  vnl_matrix<T>    I(5, 5);
  I.set_identity();
  const vnl_matrix<T> residual = svd.inverse() * H - I;
  EXPECT_NEAR(residual.fro_norm(), 0.0, residualTol);
}

// A - U diag(W) V^H == 0 for a random 5x5.
template <typename T>
void
recomposition(double residualTol, vnl_random & r)
{
  vnl_matrix<T> A(5, 5);
  fillRandom(A, r);
  const vnl_svd<T> svd(A);
  EXPECT_NEAR((A - svd.recompose()).fro_norm(), 0.0, residualTol);
}

// The nullvector x of a random 5x6 A satisfies ||A x|| == 0.
template <typename T>
void
nullvector(double residualTol, vnl_random & r)
{
  vnl_matrix<T> A(5, 6);
  fillRandom(A, r);
  const vnl_svd<T>    svd(A);
  const vnl_vector<T> x = svd.nullvector();
  EXPECT_NEAR((A * x).two_norm(), 0.0, residualTol);
}
} // namespace

TEST(VnlSVDEngine, HilbertInverse)
{
  hilbertInverse<double>(1.1e-10);
  hilbertInverse<float>(0.025);
  hilbertInverse<std::complex<double>>(4.4e-10);
  hilbertInverse<std::complex<float>>(0.04);
}

TEST(VnlSVDEngine, LeastSquaresParabola)
{
  // Recover (a,b,c) of a*x^2 + b*x + c from a noisy over-determined fit.
  constexpr double   a = 0.15;
  constexpr double   b = 1.2;
  constexpr double   c = 3.1;
  vnl_matrix<double> D(100, 3);
  vnl_vector<double> y(100);
  for (int n = 0; n < 100; ++n)
  {
    const double x = n;
    D(n, 0) = x * x;
    D(n, 1) = x;
    D(n, 2) = 1.0;
    y(n) = a * x * x + b * x + c + (n % 4 - 2) / 10.0; // sawtooth noise
  }
  const vnl_svd<double>    svd(D);
  const vnl_vector<double> params = svd.solve(y);
  vnl_vector<double>       expected(3);
  expected[0] = a;
  expected[1] = b;
  expected[2] = c;
  EXPECT_NEAR((params - expected).squared_magnitude(), 0.0, 0.005);
}

TEST(VnlSVDEngine, RankAndNullspace)
{
  // Rank-2 3x4 matrix: exercise recompose, rank, singularities, nullspace.
  double                   pdata[] = { 2, 0, 0, 0, 3, 10, 5, 5, 5, 12, 6, 6 };
  const vnl_matrix<double> P(pdata, 3, 4);
  const vnl_svd<double>    svd(P, 1e-8);

  EXPECT_NEAR((svd.recompose() - P).fro_norm(), 0.0, 1e-12);
  EXPECT_EQ(svd.singularities(), 2);
  EXPECT_EQ(svd.rank(), 2u);

  const vnl_matrix<double> nullSpace = svd.nullspace();
  EXPECT_EQ(nullSpace.columns(), 2u);
  EXPECT_NEAR((P * nullSpace).fro_norm(), 0.0, 1e-12);

  EXPECT_NEAR((P * svd.nullvector()).magnitude(), 0.0, 1e-12);
  EXPECT_NEAR((svd.left_nullvector() * P).magnitude(), 0.0, 1e-12);
}

TEST(VnlSVDEngine, IdentitySingularValues)
{
  // 3x4 [I | 0]: singular values are (1,1,1,0).
  double                   idata[] = { 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0 };
  const vnl_matrix<double> P(3, 4, 12, idata);
  const vnl_svd<double>    svd(P);
  vnl_vector<double>       expected(4);
  expected[0] = 1;
  expected[1] = 1;
  expected[2] = 1;
  expected[3] = 0;
  EXPECT_NEAR(vnl_vector_ssd(expected, svd.W().diagonal().as_ref()), 0.0, 1e-16);
}

TEST(VnlSVDEngine, Recomposition)
{
  vnl_random rng(9667566);
  recomposition<float>(1e-5, rng);
  recomposition<double>(1e-10, rng);
  recomposition<std::complex<float>>(1e-5, rng);
  recomposition<std::complex<double>>(1e-10, rng);
}

TEST(VnlSVDEngine, Nullvector)
{
  vnl_random rng(9667566);
  // Residual bounds allow for BLAS/LAPACK implementation differences across
  // platforms; near-machine-epsilon values falsely failed on non-Accelerate BLAS.
  nullvector<float>(1e-5, rng);
  nullvector<double>(1e-12, rng);
  nullvector<std::complex<float>>(1e-5, rng);
  nullvector<std::complex<double>>(1e-12, rng);
}

#endif // ITK_FUTURE_LEGACY_REMOVE
