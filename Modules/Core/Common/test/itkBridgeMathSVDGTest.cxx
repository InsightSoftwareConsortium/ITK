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
#include "itkMatrix.h"
#include "itkBridgeMathSVD.h"

// This test cross-checks against the vnl_svd reference engine; mark it so it keeps
// compiling once vnl_svd is deprecated under ITK_LEGACY_REMOVE. The oracle is
// unavailable under ITK_FUTURE_LEGACY_REMOVE.
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST
#  include "vnl/algo/vnl_svd.h"
#endif

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
TEST(BridgeMathSVD, FixedReconstructs)
{
  {
    const auto A = MakeFixed<double, 3>();
    const auto r = itk::bridge::Math::SVD(A);
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
    const auto r = itk::bridge::Math::SVD(A);
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

#ifndef ITK_FUTURE_LEGACY_REMOVE
// Singular values agree with vnl_svd (the engine being supplemented).
TEST(BridgeMathSVD, SingularValuesMatchVnl)
{
  const auto            A = MakeFixed<double, 6>();
  const auto            r = itk::bridge::Math::SVD(A);
  const vnl_svd<double> ref(A.as_matrix());
  for (unsigned int i = 0; i < 6; ++i)
    EXPECT_NEAR(r.W[i], ref.W()(i, i), 1e-12);
}
#endif // ITK_FUTURE_LEGACY_REMOVE

// Default canonicalization makes the largest-magnitude element of each U column
// positive, giving a deterministic, build-independent sign convention.
TEST(BridgeMathSVD, SignsCanonical)
{
  const auto A = MakeFixed<double, 4>();
  const auto r = itk::bridge::Math::SVD(A);
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

#ifndef ITK_FUTURE_LEGACY_REMOVE
// U V^T is invariant to the SVD sign/basis ambiguity: it matches vnl_svd even
// for a degenerate (repeated singular value) matrix. This is the property the
// geometry call sites (orthogonalization) rely on.
TEST(BridgeMathSVD, UVtransposeInvariantOnDegenerate)
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

  const auto                           r = itk::bridge::Math::SVD(A, /*canonicalizeSigns=*/false);
  const vnl_svd<double>                ref(A.as_matrix());
  const vnl_matrix_fixed<double, 3, 3> rotEig = r.U * r.V.transpose();
  const vnl_matrix<double>             rotVnl = ref.U() * ref.V().transpose();
  double                               diff = 0.0;
  for (unsigned int i = 0; i < 3; ++i)
    for (unsigned int j = 0; j < 3; ++j)
      diff = std::max(diff, std::abs(rotEig(i, j) - rotVnl(i, j)));
  EXPECT_LT(diff, 1e-12);
}
#endif // ITK_FUTURE_LEGACY_REMOVE

// The itk::Matrix overload forwards to the same computation.
TEST(BridgeMathSVD, ItkMatrixOverload)
{
  itk::Matrix<double, 3, 3> A;
  A.SetIdentity();
  A(0, 2) = 0.5;
  const auto r = itk::bridge::Math::SVD(A);
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
TEST(BridgeMathSVD, DynamicReconstructs)
{
  for (const unsigned int n : { 3u, 6u, 7u, 20u }) // 6 -> JacobiSVD, 7 -> BDCSVD (crossover)
  {
    vnl_matrix<double> A(n, n);
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = 0; j < n; ++j)
        A(i, j) = std::sin(0.7 * i + 1.3 * j) + (i == j ? 3.0 : 0.0);

    const auto r = itk::bridge::Math::SVD(A);
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

#ifndef ITK_FUTURE_LEGACY_REMOVE
    const vnl_svd<double> ref(A);
    for (unsigned int i = 0; i < n; ++i)
      EXPECT_NEAR(r.W[i], ref.W()(i, i), 1e-11) << "n=" << n << " i=" << i;
#endif
  }
}

// An empty runtime input is rejected.
TEST(BridgeMathSVD, DynamicRejectsEmpty)
{
  const vnl_matrix<double> A(0, 0);
  EXPECT_THROW(itk::bridge::Math::SVD(A), itk::ExceptionObject);
}

// PseudoInverse() agrees with vnl_svd and satisfies the Moore-Penrose identity.
TEST(BridgeMathSVD, PseudoInverseMatchesVnl)
{
  const auto A = MakeFixed<double, 6>();
  const auto pinv = itk::bridge::Math::SVD(A).PseudoInverse();
#ifndef ITK_FUTURE_LEGACY_REMOVE
  const vnl_matrix<double> pinvVnl = vnl_svd<double>(A.as_matrix()).pinverse();
  double                   dInv = 0.0;
  for (unsigned int i = 0; i < 6; ++i)
    for (unsigned int j = 0; j < 6; ++j)
      dInv = std::max(dInv, std::abs(pinv(i, j) - pinvVnl(i, j)));
  EXPECT_LT(dInv, 1e-10);
#endif

  // A * A^+ * A == A
  const vnl_matrix_fixed<double, 6, 6> recon = A * pinv * A;
  double                               dId = 0.0;
  for (unsigned int i = 0; i < 6; ++i)
    for (unsigned int j = 0; j < 6; ++j)
      dId = std::max(dId, std::abs(recon(i, j) - A(i, j)));
  EXPECT_LT(dId, 1e-10);
}

// Solve() returns the solution of A x = b for a well-conditioned A.
TEST(BridgeMathSVD, SolveMatchesVnl)
{
  const auto                  A = MakeFixed<double, 4>();
  vnl_vector_fixed<double, 4> b;
  for (unsigned int i = 0; i < 4; ++i)
    b[i] = static_cast<double>(i + 1);

  const auto x = itk::bridge::Math::SVD(A).Solve(b);

  // residual A x - b is ~0 for a full-rank A
  const vnl_vector_fixed<double, 4> residual = A * x - b;
  EXPECT_LT(residual.inf_norm(), 1e-10);

#ifndef ITK_FUTURE_LEGACY_REMOVE
  // agrees with vnl_svd's solve
  const vnl_vector<double> xVnl = vnl_svd<double>(A.as_matrix()).solve(b.as_vector());
  EXPECT_LT((x.as_vector() - xVnl).inf_norm(), 1e-10);
#endif
}

#ifndef ITK_FUTURE_LEGACY_REMOVE
// WellCondition() (sigma_min/sigma_max) agrees with vnl_svd::well_condition(),
// the accessor the NIfTI direction-cosine check migrated onto.
TEST(BridgeMathSVD, WellConditionMatchesVnl)
{
  const auto A = MakeFixed<double, 5>();
  EXPECT_NEAR(itk::bridge::Math::SVD(A).WellCondition(), vnl_svd<double>(A.as_matrix()).well_condition(), 1e-12);
}

// DeterminantMagnitude() (product of singular values) agrees with
// vnl_svd::determinant_magnitude(), the accessor the Mahalanobis check migrated onto.
TEST(BridgeMathSVD, DeterminantMagnitudeMatchesVnl)
{
  const auto   A = MakeFixed<double, 5>();
  const double itkDet = itk::bridge::Math::SVD(A).DeterminantMagnitude();
  const double vnlDet = vnl_svd<double>(A.as_matrix()).determinant_magnitude();
  EXPECT_NEAR(itkDet, vnlDet, 1e-10 * std::abs(vnlDet));
}
#endif // ITK_FUTURE_LEGACY_REMOVE

// rank() reports full rank for a well-conditioned matrix and the reduced rank of
// a deliberately rank-deficient one.
TEST(BridgeMathSVD, Rank)
{
  EXPECT_EQ(itk::bridge::Math::SVD(MakeFixed<double, 6>()).Rank(), 6u);

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
  EXPECT_EQ(itk::bridge::Math::SVD(A).Rank(), 2u);
}

// Ill-conditioned input (6x6 Hilbert, condition ~1e7): reconstruction stays
// accurate and the singular values still agree with vnl_svd. Stresses the engines
// where Jacobi and BDC can diverge, which the well-conditioned fixtures do not.
TEST(BridgeMathSVD, IllConditionedMatchesVnl)
{
  constexpr unsigned int N = 6;
  vnl_matrix<double>     A(N, N);
  for (unsigned int i = 0; i < N; ++i)
    for (unsigned int j = 0; j < N; ++j)
      A(i, j) = 1.0 / (i + j + 1.0); // Hilbert

  const auto r = itk::bridge::Math::SVD(A);

#ifndef ITK_FUTURE_LEGACY_REMOVE
  // singular values agree relative to the largest
  const vnl_svd<double> ref(A);
  const double          wmax = r.W[0];
  for (unsigned int i = 0; i < N; ++i)
    EXPECT_NEAR(r.W[i], ref.W()(i, i), 1e-9 * wmax) << "i=" << i;
#endif

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
TEST(BridgeMathSVD, CanonicalizeFlagControlsSigns)
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

    const auto rTrue = itk::bridge::Math::SVD(A, true);
    const auto rFalse = itk::bridge::Math::SVD(A, false);

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
TEST(BridgeMathSVD, DynamicSignsCanonical)
{
  vnl_matrix<double> A(5, 5);
  for (unsigned int i = 0; i < 5; ++i)
    for (unsigned int j = 0; j < 5; ++j)
      A(i, j) = std::sin(0.9 * i + 1.7 * j) + (i == j ? 2.0 : 0.0);

  const auto r = itk::bridge::Math::SVD(A);
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
TEST(BridgeMathSVD, DegenerateSpectrumReconstructs)
{
  // diag(2,2,2,5): a triply-repeated singular value.
  vnl_matrix<double> A(4, 4, 0.0);
  A(0, 0) = 2.0;
  A(1, 1) = 2.0;
  A(2, 2) = 2.0;
  A(3, 3) = 5.0;
  const auto r = itk::bridge::Math::SVD(A);
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

// Recompose() with no truncation reconstructs A; a threshold zeroes the small
// singular value, yielding the rank-reduced reconstruction.
TEST(BridgeMathSVD, Recompose)
{
  const auto A = MakeFixed<double, 4>();
  const auto recon = itk::bridge::Math::SVD(A).Recompose();
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
  const auto truncated = itk::bridge::Math::SVD(B).Recompose(1e-6);
  EXPECT_NEAR(truncated(0, 0), 5.0, 1e-10);
  EXPECT_NEAR(truncated(1, 1), 3.0, 1e-10);
  EXPECT_NEAR(truncated(2, 2), 0.0, 1e-12);
}

// Rectangular (tall and wide): thin factors reconstruct A, shapes are correct,
// and PseudoInverse()/Solve() match vnl_svd and satisfy the Moore-Penrose identity.
TEST(BridgeMathSVD, Rectangular)
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

    const auto r = itk::bridge::Math::SVD(A);
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

    // PseudoInverse is n x m and matches vnl_svd
    const auto pinv = r.PseudoInverse();
    EXPECT_EQ(pinv.rows(), n);
    EXPECT_EQ(pinv.cols(), m);
#ifndef ITK_FUTURE_LEGACY_REMOVE
    const vnl_matrix<double> pinvVnl = vnl_svd<double>(A).pinverse();
    double                   pinvErr = 0.0;
    for (unsigned int i = 0; i < n; ++i)
      for (unsigned int j = 0; j < m; ++j)
        pinvErr = std::max(pinvErr, std::abs(pinv(i, j) - pinvVnl(i, j)));
    EXPECT_LT(pinvErr, 1e-10) << "m=" << m << " n=" << n;
#endif

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
TEST(BridgeMathSVD, RectangularSolveMatchesVnl)
{
  vnl_matrix<double> A(5, 3); // overdetermined
  for (unsigned int i = 0; i < 5; ++i)
    for (unsigned int j = 0; j < 3; ++j)
      A(i, j) = std::cos(0.4 * i + 0.9 * j) + (i == j ? 2.0 : 0.0);
  vnl_vector<double> b(5);
  for (unsigned int i = 0; i < 5; ++i)
    b[i] = static_cast<double>(i) - 1.5;

  const vnl_vector<double> x = itk::bridge::Math::SVD(A).Solve(b);
  EXPECT_EQ(x.size(), 3u);
#ifndef ITK_FUTURE_LEGACY_REMOVE
  const vnl_vector<double> xVnl = vnl_svd<double>(A).solve(b);
  EXPECT_LT((x - xVnl).inf_norm(), 1e-10);
#endif
}

// NullVector() matches vnl_svd::nullvector() up to sign, and lies in the nullspace.
TEST(BridgeMathSVD, NullVector)
{
  // Rank-2 square 3x3: third row = row0 + row1.
  vnl_matrix<double> A(3, 3);
  A(0, 0) = 1.0;
  A(0, 1) = 2.0;
  A(0, 2) = 3.0;
  A(1, 0) = 4.0;
  A(1, 1) = 5.0;
  A(1, 2) = 6.0;
  for (unsigned int j = 0; j < 3; ++j)
    A(2, j) = A(0, j) + A(1, j);

  const vnl_vector<double> nv = itk::bridge::Math::SVD(A).NullVector();
  EXPECT_LT((A * nv).inf_norm(), 1e-12);

#ifndef ITK_FUTURE_LEGACY_REMOVE
  const vnl_vector<double> nvVnl = vnl_svd<double>(A).nullvector();
  const double             align = std::abs(dot_product(nv, nvVnl)) / (nv.magnitude() * nvVnl.magnitude());
  EXPECT_NEAR(align, 1.0, 1e-12);
#endif

  // Fixed-size overload agrees.
  vnl_matrix_fixed<double, 3, 3> Af;
  Af.copy_in(A.data_block());
  const vnl_vector_fixed<double, 3> nvf = itk::bridge::Math::SVD(Af).NullVector();
  EXPECT_LT((A * nvf.as_vector()).inf_norm(), 1e-12);
}

// NullVector() refuses an underdetermined input whose thin V cannot span the nullspace.
TEST(BridgeMathSVD, NullVectorRejectsUnderdetermined)
{
  vnl_matrix<double> A(2, 4);
  A.fill(1.0);
  A(0, 1) = 2.0;
  A(1, 3) = 3.0;
  EXPECT_THROW(itk::bridge::Math::SVD(A).NullVector(), itk::ExceptionObject);
}

// RecomposeWith() reproduces A for unmodified W and honors hand-edited values.
TEST(BridgeMathSVD, RecomposeWith)
{
  const auto Af = MakeFixed<double, 4>();
  const auto r = itk::bridge::Math::SVD(Af);

  const vnl_matrix_fixed<double, 4, 4> same = r.RecomposeWith(r.W);
  double                               err = 0.0;
  for (unsigned int i = 0; i < 4; ++i)
    for (unsigned int j = 0; j < 4; ++j)
      err = std::max(err, std::abs(same(i, j) - Af(i, j)));
  EXPECT_LT(err, 1e-12);

  // Zeroing the tail value reproduces the rank-truncated Recompose().
  vnl_vector_fixed<double, 4> wMod = r.W;
  wMod[3] = 0.0;
  const vnl_matrix_fixed<double, 4, 4> truncated = r.RecomposeWith(wMod);
  // Truncate via rcond just above the smallest normalized singular value.
  const double                         rcond = (r.W[3] / r.W[0]) * (1.0 + 1e-6);
  const vnl_matrix_fixed<double, 4, 4> viaRcond = r.Recompose(rcond);
  for (unsigned int i = 0; i < 4; ++i)
    for (unsigned int j = 0; j < 4; ++j)
      EXPECT_NEAR(truncated(i, j), viaRcond(i, j), 1e-12);

  // Inverted values give the transposed pseudo-inverse: U diag(1/w) V^T == (A^+)^T.
  vnl_vector_fixed<double, 4> wInv;
  for (unsigned int k = 0; k < 4; ++k)
    wInv[k] = 1.0 / r.W[k];
  const vnl_matrix_fixed<double, 4, 4> recomposedInv = r.RecomposeWith(wInv);
  const vnl_matrix_fixed<double, 4, 4> pinvT = r.PseudoInverse(0.0).transpose();
  for (unsigned int i = 0; i < 4; ++i)
    for (unsigned int j = 0; j < 4; ++j)
      EXPECT_NEAR(recomposedInv(i, j), pinvT(i, j), 1e-10);
}

// Fixed rectangular overload: factors match the dynamic path and the
// pseudo-inverse matches the vnl_svd reference (the itkTransform Jacobian use).
TEST(BridgeMathSVD, FixedRectangular)
{
  constexpr unsigned int               rows = 5;
  constexpr unsigned int               cols = 3;
  vnl_matrix_fixed<double, rows, cols> Af;
  for (unsigned int i = 0; i < rows; ++i)
    for (unsigned int j = 0; j < cols; ++j)
      Af(i, j) = std::cos(0.4 * i + 0.9 * j) + (i == j ? 2.0 : 0.0);

  const auto rf = itk::bridge::Math::SVD(Af);
  const auto rd = itk::bridge::Math::SVD(vnl_matrix<double>(Af.as_matrix()));
  for (unsigned int k = 0; k < 3; ++k)
    EXPECT_NEAR(rf.W[k], rd.W[k], 1e-12);

#ifndef ITK_FUTURE_LEGACY_REMOVE
  const vnl_matrix_fixed<double, cols, rows> pinv = rf.PseudoInverse();
  const vnl_matrix<double>                   pinvVnl = vnl_svd<double>(Af.as_matrix()).inverse();
  for (unsigned int i = 0; i < cols; ++i)
    for (unsigned int j = 0; j < rows; ++j)
      EXPECT_NEAR(pinv(i, j), pinvVnl(i, j), 1e-10);
#endif

  // NullVector on an overdetermined fixed shape lies in the nullspace of a
  // rank-deficient input (third column = sum of the first two).
  vnl_matrix_fixed<double, rows, cols> Adef = Af;
  for (unsigned int i = 0; i < rows; ++i)
    Adef(i, 2) = Adef(i, 0) + Adef(i, 1);
  const vnl_vector_fixed<double, cols> nv = itk::bridge::Math::SVD(Adef).NullVector();
  EXPECT_LT((Adef.as_matrix() * nv.as_vector()).inf_norm(), 1e-12);
}
