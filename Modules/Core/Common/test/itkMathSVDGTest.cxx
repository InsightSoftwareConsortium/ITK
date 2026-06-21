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
#include "itkMathSVD.h"

// This test cross-checks against the vnl_svd reference engine; mark it so it keeps
// compiling once vnl_svd is deprecated under ITK_LEGACY_REMOVE.
#define ITK_LEGACY_TEST
#include "vnl/algo/vnl_svd.h"

#include <gtest/gtest.h>
#include <cmath>
#include <algorithm>

namespace
{
// Well-conditioned non-symmetric square matrix with a strong diagonal.
template <typename T, unsigned int VDim>
vnl_matrix_fixed<T, VDim, VDim>
MakeFixed()
{
  vnl_matrix_fixed<T, VDim, VDim> A;
  for (unsigned int i = 0; i < VDim; ++i)
    for (unsigned int j = 0; j < VDim; ++j)
      A(i, j) = static_cast<T>(std::sin(0.7 * i + 1.3 * j) + (i == j ? 3.0 : 0.0));
  return A;
}
} // namespace

// A == U diag(W) V^T for fixed-size square matrices, float and double.
TEST(MathSVD, FixedReconstructs)
{
  {
    const auto A = MakeFixed<double, 3>();
    const auto r = itk::Math::SVD(A);
    double     err{};
    for (unsigned int i = 0; i < 3; ++i)
      for (unsigned int j = 0; j < 3; ++j)
      {
        double acc{};
        for (unsigned int k = 0; k < 3; ++k)
          acc += r.U(i, k) * r.W[k] * r.V(j, k);
        err = std::max(err, std::abs(acc - A(i, j)));
      }
    EXPECT_LT(err, 1e-12);
  }
  {
    const auto A = MakeFixed<float, 6>();
    const auto r = itk::Math::SVD(A);
    float      err{};
    for (unsigned int i = 0; i < 6; ++i)
      for (unsigned int j = 0; j < 6; ++j)
      {
        float acc{};
        for (unsigned int k = 0; k < 6; ++k)
          acc += r.U(i, k) * r.W[k] * r.V(j, k);
        err = std::max(err, std::abs(acc - A(i, j)));
      }
    EXPECT_LT(err, 1e-5f);
  }
}

// Singular values agree with vnl_svd (the engine being supplemented).
TEST(MathSVD, SingularValuesMatchVnl)
{
  const auto            A = MakeFixed<double, 6>();
  const auto            r = itk::Math::SVD(A);
  const vnl_svd<double> ref(A.as_matrix());
  for (unsigned int i = 0; i < 6; ++i)
    EXPECT_NEAR(r.W[i], ref.W()(i, i), 1e-12);
}

// Default canonicalization makes the largest-magnitude element of each U column
// positive, giving a deterministic, build-independent sign convention.
TEST(MathSVD, SignsCanonical)
{
  const auto A = MakeFixed<double, 4>();
  const auto r = itk::Math::SVD(A);
  for (unsigned int j = 0; j < 4; ++j)
  {
    unsigned int pivot = 0;
    double       best = 0.0;
    for (unsigned int i = 0; i < 4; ++i)
      if (std::abs(r.U(i, j)) > best)
      {
        best = std::abs(r.U(i, j));
        pivot = i;
      }
    EXPECT_GT(r.U(pivot, j), 0.0);
  }
}

// U V^T is invariant to the SVD sign/basis ambiguity: it matches vnl_svd even
// for a degenerate (repeated singular value) matrix. This is the property the
// geometry call sites (orthogonalization) rely on.
TEST(MathSVD, UVtransposeInvariantOnDegenerate)
{
  // Rotation * diag(1,1,3): two equal singular values (degenerate subspace).
  vnl_matrix_fixed<double, 3, 3> R;
  const double                   a = 0.6;
  const double                   c = std::cos(a);
  const double                   s = std::sin(a);
  R(0, 0) = c;
  R(0, 1) = -s;
  R(0, 2) = 0;
  R(1, 0) = s;
  R(1, 1) = c;
  R(1, 2) = 0;
  R(2, 0) = 0;
  R(2, 1) = 0;
  R(2, 2) = 1;
  vnl_matrix_fixed<double, 3, 3> A;
  for (unsigned int i = 0; i < 3; ++i)
  {
    A(i, 0) = R(i, 0) * 1.0;
    A(i, 1) = R(i, 1) * 1.0;
    A(i, 2) = R(i, 2) * 3.0;
  }

  const auto                           r = itk::Math::SVD(A, /*canonicalizeSigns=*/false);
  const vnl_svd<double>                ref(A.as_matrix());
  const vnl_matrix_fixed<double, 3, 3> rotEig = r.U * r.V.transpose();
  const vnl_matrix<double>             rotVnl = ref.U() * ref.V().transpose();
  double                               diff = 0.0;
  for (unsigned int i = 0; i < 3; ++i)
    for (unsigned int j = 0; j < 3; ++j)
      diff = std::max(diff, std::abs(rotEig(i, j) - rotVnl(i, j)));
  EXPECT_LT(diff, 1e-12);
}

// The itk::Matrix overload forwards to the same computation.
TEST(MathSVD, ItkMatrixOverload)
{
  itk::Matrix<double, 3, 3> A;
  A.SetIdentity();
  A(0, 2) = 0.5;
  const auto r = itk::Math::SVD(A);
  double     err{};
  for (unsigned int i = 0; i < 3; ++i)
    for (unsigned int j = 0; j < 3; ++j)
    {
      double acc{};
      for (unsigned int k = 0; k < 3; ++k)
        acc += r.U(i, k) * r.W[k] * r.V(j, k);
      err = std::max(err, std::abs(acc - A(i, j)));
    }
  EXPECT_LT(err, 1e-12);
}

// Runtime-sized square overload, exercising both the small (JacobiSVD) and large
// (BDCSVD) engine branches.
TEST(MathSVD, DynamicReconstructs)
{
  for (const unsigned int n : { 3u, 6u, 7u, 20u }) // 6 -> JacobiSVD, 7 -> BDCSVD (crossover)
  {
    vnl_matrix<double> A(n, n);
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = 0; j < n; ++j)
        A(i, j) = std::sin(0.7 * i + 1.3 * j) + (i == j ? 3.0 : 0.0);

    const auto r = itk::Math::SVD(A);
    double     err{};
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = 0; j < n; ++j)
      {
        double acc{};
        for (unsigned int k = 0; k < n; ++k)
          acc += r.U(i, k) * r.W[k] * r.V(j, k);
        err = std::max(err, std::abs(acc - A(i, j)));
      }
    EXPECT_LT(err, 1e-11) << "n=" << n;

    const vnl_svd<double> ref(A);
    for (unsigned int i = 0; i < n; ++i)
      EXPECT_NEAR(r.W[i], ref.W()(i, i), 1e-11) << "n=" << n << " i=" << i;
  }
}

// An empty runtime input is rejected.
TEST(MathSVD, DynamicRejectsEmpty)
{
  const vnl_matrix<double> A(0, 0);
  EXPECT_THROW(itk::Math::SVD(A), itk::ExceptionObject);
}

// pinverse() agrees with vnl_svd and satisfies the Moore-Penrose identity.
TEST(MathSVD, PseudoInverseMatchesVnl)
{
  const auto               A = MakeFixed<double, 6>();
  const auto               pinv = itk::Math::SVD(A).pinverse();
  const vnl_matrix<double> pinvVnl = vnl_svd<double>(A.as_matrix()).pinverse();
  double                   dInv = 0.0;
  for (unsigned int i = 0; i < 6; ++i)
    for (unsigned int j = 0; j < 6; ++j)
      dInv = std::max(dInv, std::abs(pinv(i, j) - pinvVnl(i, j)));
  EXPECT_LT(dInv, 1e-10);

  // A * A^+ * A == A
  const vnl_matrix_fixed<double, 6, 6> recon = A * pinv * A;
  double                               dId = 0.0;
  for (unsigned int i = 0; i < 6; ++i)
    for (unsigned int j = 0; j < 6; ++j)
      dId = std::max(dId, std::abs(recon(i, j) - A(i, j)));
  EXPECT_LT(dId, 1e-10);
}

// Solve() returns the solution of A x = b for a well-conditioned A.
TEST(MathSVD, SolveMatchesVnl)
{
  const auto                  A = MakeFixed<double, 4>();
  vnl_vector_fixed<double, 4> b;
  for (unsigned int i = 0; i < 4; ++i)
    b[i] = static_cast<double>(i + 1);

  const auto x = itk::Math::SVD(A).Solve(b);

  // residual A x - b is ~0 for a full-rank A
  const vnl_vector_fixed<double, 4> residual = A * x - b;
  EXPECT_LT(residual.inf_norm(), 1e-10);

  // agrees with vnl_svd's solve
  const vnl_vector<double> xVnl = vnl_svd<double>(A.as_matrix()).solve(b.as_vector());
  EXPECT_LT((x.as_vector() - xVnl).inf_norm(), 1e-10);
}

// rank() reports full rank for a well-conditioned matrix and the reduced rank of
// a deliberately rank-deficient one.
TEST(MathSVD, Rank)
{
  EXPECT_EQ(itk::Math::SVD(MakeFixed<double, 6>()).rank(), 6u);

  // Two identical rows -> rank 2 for a 3x3.
  vnl_matrix<double> A(3, 3, 0.0);
  A(0, 0) = 1.0;
  A(0, 1) = 2.0;
  A(0, 2) = 3.0;
  A(1, 0) = 1.0;
  A(1, 1) = 2.0;
  A(1, 2) = 3.0;
  A(2, 0) = 4.0;
  A(2, 1) = 0.0;
  A(2, 2) = 1.0;
  EXPECT_EQ(itk::Math::SVD(A).rank(), 2u);
}

// Ill-conditioned input (6x6 Hilbert, condition ~1e7): reconstruction stays
// accurate and the singular values still agree with vnl_svd. Stresses the engines
// where Jacobi and BDC can diverge, which the well-conditioned fixtures do not.
TEST(MathSVD, IllConditionedMatchesVnl)
{
  constexpr unsigned int N = 6;
  vnl_matrix<double>     A(N, N);
  for (unsigned int i = 0; i < N; ++i)
    for (unsigned int j = 0; j < N; ++j)
      A(i, j) = 1.0 / (i + j + 1.0); // Hilbert

  const auto            r = itk::Math::SVD(A);
  const vnl_svd<double> ref(A);

  // singular values agree relative to the largest
  const double wmax = r.W[0];
  for (unsigned int i = 0; i < N; ++i)
    EXPECT_NEAR(r.W[i], ref.W()(i, i), 1e-9 * wmax) << "i=" << i;

  // reconstruction A == U diag(W) V^T is well-conditioned and stays ~machine eps
  double err = 0.0;
  for (unsigned int i = 0; i < N; ++i)
    for (unsigned int j = 0; j < N; ++j)
    {
      double acc = 0.0;
      for (unsigned int k = 0; k < N; ++k)
        acc += r.U(i, k) * r.W[k] * r.V(j, k);
      err = std::max(err, std::abs(acc - A(i, j)));
    }
  EXPECT_LT(err, 1e-12);
}

// canonicalizeSigns actually controls the output (a test that fails if the flag
// were ignored): true yields canonical signs and differs from the raw false result.
TEST(MathSVD, CanonicalizeFlagControlsSigns)
{
  // Scan several matrices: true must always be canonical, false must preserve
  // Eigen's raw signs, and at least one raw column must be non-canonical (proving
  // false really skips canonicalization). Fails if the flag were ignored either
  // way: never-canonicalize trips the rTrue-canonical check on a raw-negative
  // column; always-canonicalize leaves sawNonCanonical false.
  bool sawNonCanonical = false;
  for (int seed = 0; seed < 12; ++seed)
  {
    vnl_matrix_fixed<double, 4, 4> A;
    for (unsigned int i = 0; i < 4; ++i)
      for (unsigned int j = 0; j < 4; ++j)
        A(i, j) = std::sin(0.37 * seed + 0.7 * i + 1.9 * j) + (i == j ? 1.0 : 0.0);

    const auto rTrue = itk::Math::SVD(A, true);
    const auto rFalse = itk::Math::SVD(A, false);

    for (unsigned int j = 0; j < 4; ++j)
    {
      unsigned int pTrue = 0;
      unsigned int pFalse = 0;
      for (unsigned int i = 1; i < 4; ++i)
      {
        if (std::abs(rTrue.U(i, j)) > std::abs(rTrue.U(pTrue, j)))
          pTrue = i;
        if (std::abs(rFalse.U(i, j)) > std::abs(rFalse.U(pFalse, j)))
          pFalse = i;
      }
      EXPECT_GT(rTrue.U(pTrue, j), 0.0) << "seed " << seed << " col " << j;

      const bool falseNegative = rFalse.U(pFalse, j) < 0.0;
      sawNonCanonical = sawNonCanonical || falseNegative;
      for (unsigned int i = 0; i < 4; ++i)
      {
        // where false is non-canonical, true is its negation; else they agree.
        const double expectedU = falseNegative ? -rFalse.U(i, j) : rFalse.U(i, j);
        const double expectedV = falseNegative ? -rFalse.V(i, j) : rFalse.V(i, j);
        EXPECT_NEAR(rTrue.U(i, j), expectedU, 1e-12);
        EXPECT_NEAR(rTrue.V(i, j), expectedV, 1e-12);
      }
    }
  }
  EXPECT_TRUE(sawNonCanonical) << "expected at least one non-canonical raw Eigen column";
}

// The runtime overload also canonicalizes (the dynamic SvdFlip path).
TEST(MathSVD, DynamicSignsCanonical)
{
  vnl_matrix<double> A(5, 5);
  for (unsigned int i = 0; i < 5; ++i)
    for (unsigned int j = 0; j < 5; ++j)
      A(i, j) = std::sin(0.9 * i + 1.7 * j) + (i == j ? 2.0 : 0.0);

  const auto r = itk::Math::SVD(A);
  for (unsigned int j = 0; j < 5; ++j)
  {
    unsigned int pivot = 0;
    for (unsigned int i = 1; i < 5; ++i)
      if (std::abs(r.U(i, j)) > std::abs(r.U(pivot, j)))
        pivot = i;
    EXPECT_GT(r.U(pivot, j), 0.0) << "col " << j;
  }
}

// Repeated singular values (degenerate spectrum) still reconstruct exactly, even
// though the singular-vector basis within the degenerate subspace is arbitrary.
TEST(MathSVD, DegenerateSpectrumReconstructs)
{
  // diag(2,2,2,5): a triply-repeated singular value.
  vnl_matrix<double> A(4, 4, 0.0);
  A(0, 0) = 2.0;
  A(1, 1) = 2.0;
  A(2, 2) = 2.0;
  A(3, 3) = 5.0;
  const auto r = itk::Math::SVD(A);
  double     err = 0.0;
  for (unsigned int i = 0; i < 4; ++i)
    for (unsigned int j = 0; j < 4; ++j)
    {
      double acc = 0.0;
      for (unsigned int k = 0; k < 4; ++k)
        acc += r.U(i, k) * r.W[k] * r.V(j, k);
      err = std::max(err, std::abs(acc - A(i, j)));
    }
  EXPECT_LT(err, 1e-12);
}

// recompose() with no truncation reconstructs A; a threshold zeroes the small
// singular value, yielding the rank-reduced reconstruction.
TEST(MathSVD, Recompose)
{
  const auto A = MakeFixed<double, 4>();
  const auto recon = itk::Math::SVD(A).recompose();
  double     err = 0.0;
  for (unsigned int i = 0; i < 4; ++i)
    for (unsigned int j = 0; j < 4; ++j)
      err = std::max(err, std::abs(recon(i, j) - A(i, j)));
  EXPECT_LT(err, 1e-12);

  // diag(5, 3, 1e-9): a relative threshold of 1e-6 drops the 1e-9 singular value.
  vnl_matrix<double> B(3, 3, 0.0);
  B(0, 0) = 5.0;
  B(1, 1) = 3.0;
  B(2, 2) = 1e-9;
  const auto truncated = itk::Math::SVD(B).recompose(1e-6);
  EXPECT_NEAR(truncated(0, 0), 5.0, 1e-10);
  EXPECT_NEAR(truncated(1, 1), 3.0, 1e-10);
  EXPECT_NEAR(truncated(2, 2), 0.0, 1e-12);
}

// Rectangular (tall and wide): thin factors reconstruct A, shapes are correct,
// and pinverse()/Solve() match vnl_svd and satisfy the Moore-Penrose identity.
TEST(MathSVD, Rectangular)
{
  const unsigned int dims[2][2] = { { 5, 3 }, { 3, 5 } }; // tall, wide
  for (const auto & d : dims)
  {
    const unsigned int m = d[0];
    const unsigned int n = d[1];
    const unsigned int k = std::min(m, n);
    vnl_matrix<double> A(m, n);
    for (unsigned int i = 0; i < m; ++i)
      for (unsigned int j = 0; j < n; ++j)
        A(i, j) = std::sin(0.6 * i + 1.1 * j) + (i == j ? 1.0 : 0.0);

    const auto r = itk::Math::SVD(A);
    EXPECT_EQ(r.U.rows(), m);
    EXPECT_EQ(r.U.cols(), k);
    EXPECT_EQ(r.V.rows(), n);
    EXPECT_EQ(r.V.cols(), k);
    EXPECT_EQ(r.W.size(), k);

    // A == U diag(W) V^T (thin)
    double reconErr = 0.0;
    for (unsigned int i = 0; i < m; ++i)
      for (unsigned int j = 0; j < n; ++j)
      {
        double acc = 0.0;
        for (unsigned int c = 0; c < k; ++c)
          acc += r.U(i, c) * r.W[c] * r.V(j, c);
        reconErr = std::max(reconErr, std::abs(acc - A(i, j)));
      }
    EXPECT_LT(reconErr, 1e-12) << "m=" << m << " n=" << n;

    // pinverse is n x m and matches vnl_svd
    const auto               pinv = r.pinverse();
    const vnl_matrix<double> pinvVnl = vnl_svd<double>(A).pinverse();
    EXPECT_EQ(pinv.rows(), n);
    EXPECT_EQ(pinv.cols(), m);
    double pinvErr = 0.0;
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = 0; j < m; ++j)
        pinvErr = std::max(pinvErr, std::abs(pinv(i, j) - pinvVnl(i, j)));
    EXPECT_LT(pinvErr, 1e-10) << "m=" << m << " n=" << n;

    // Moore-Penrose: A A^+ A == A
    const vnl_matrix<double> recon = A * pinv * A;
    double                   mpErr = 0.0;
    for (unsigned int i = 0; i < m; ++i)
      for (unsigned int j = 0; j < n; ++j)
        mpErr = std::max(mpErr, std::abs(recon(i, j) - A(i, j)));
    EXPECT_LT(mpErr, 1e-10) << "m=" << m << " n=" << n;
  }
}

// Rectangular least-squares Solve() matches vnl_svd (the BSpline refinement use).
TEST(MathSVD, RectangularSolveMatchesVnl)
{
  vnl_matrix<double> A(5, 3); // overdetermined
  for (unsigned int i = 0; i < 5; ++i)
    for (unsigned int j = 0; j < 3; ++j)
      A(i, j) = std::cos(0.4 * i + 0.9 * j) + (i == j ? 2.0 : 0.0);
  vnl_vector<double> b(5);
  for (unsigned int i = 0; i < 5; ++i)
    b[i] = static_cast<double>(i) - 1.5;

  const vnl_vector<double> x = itk::Math::SVD(A).Solve(b);
  const vnl_vector<double> xVnl = vnl_svd<double>(A).solve(b);
  EXPECT_EQ(x.size(), 3u);
  EXPECT_LT((x - xVnl).inf_norm(), 1e-10);
}
