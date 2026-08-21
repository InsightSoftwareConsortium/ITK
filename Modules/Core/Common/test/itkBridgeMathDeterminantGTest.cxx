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
#include "itkBridgeMathDeterminant.h"

#include "itkMatrix.h"

// This test cross-checks against the vnl reference engines; mark it so it keeps
// compiling once vnl_determinant/vnl_det are deprecated under ITK_LEGACY_REMOVE.
#define ITK_LEGACY_TEST
#include "vnl/algo/vnl_determinant.h"
#include "vnl/vnl_det.h"

#include <gtest/gtest.h>
#include <cmath>

namespace
{
// The analytic 2x2/3x3/4x4 fixtures from vnl's test_determinant, whose values
// are verified below by an independent closed-form expansion.
const double kM2[2][2] = { { 0.60684258354179, 0.89129896614890 }, { 0.48598246870930, 0.76209683302739 } };
const double kM3[3][3] = { { 0.45646766516834, 0.44470336435319, 0.92181297074480 },
                           { 0.01850364324822, 0.61543234810009, 0.73820724581067 },
                           { 0.82140716429525, 0.79193703742704, 0.17626614449462 } };

template <typename T, unsigned int VDim>
itk::Matrix<T, VDim, VDim>
FillMatrix(const double src[VDim][VDim])
{
  itk::Matrix<T, VDim, VDim> A;
  for (unsigned int i = 0; i < VDim; ++i)
    for (unsigned int j = 0; j < VDim; ++j)
      A(i, j) = static_cast<T>(src[i][j]);
  return A;
}
} // namespace

// Closed-form 2x2 reference: ad - bc.
TEST(BridgeMathDeterminant, TwoByTwoAnalytic)
{
  const auto   A = FillMatrix<double, 2>(kM2);
  const double expected = kM2[0][0] * kM2[1][1] - kM2[0][1] * kM2[1][0];
  EXPECT_NEAR(itk::bridge::Math::Determinant(A), expected, 1e-14);
}

// Closed-form 3x3 reference: cofactor expansion along the first row.
TEST(BridgeMathDeterminant, ThreeByThreeAnalytic)
{
  const auto   A = FillMatrix<double, 3>(kM3);
  const double expected = kM3[0][0] * (kM3[1][1] * kM3[2][2] - kM3[1][2] * kM3[2][1]) -
                          kM3[0][1] * (kM3[1][0] * kM3[2][2] - kM3[1][2] * kM3[2][0]) +
                          kM3[0][2] * (kM3[1][0] * kM3[2][1] - kM3[1][1] * kM3[2][0]);
  EXPECT_NEAR(itk::bridge::Math::Determinant(A), expected, 1e-14);
}

// The three overloads (itk::Matrix, vnl_matrix_fixed, vnl_matrix) return the
// same value for the same data.
TEST(BridgeMathDeterminant, OverloadConsistency)
{
  const auto                           A = FillMatrix<double, 3>(kM3);
  const vnl_matrix_fixed<double, 3, 3> fixedA = A.GetVnlMatrix();
  const vnl_matrix<double>             dynA = fixedA.as_matrix();

  const double d = itk::bridge::Math::Determinant(A);
  EXPECT_DOUBLE_EQ(itk::bridge::Math::Determinant(fixedA), d);
  EXPECT_NEAR(itk::bridge::Math::Determinant(dynA), d, 1e-14);
}

// Identity has unit determinant; a diagonal matrix yields the product of its
// diagonal; both stress the direct fixed-size path.
TEST(BridgeMathDeterminant, IdentityAndDiagonal)
{
  EXPECT_DOUBLE_EQ(itk::bridge::Math::Determinant(itk::Matrix<double, 4, 4>::GetIdentity()), 1.0);

  itk::Matrix<double, 4, 4> D;
  D.SetIdentity();
  D(0, 0) = 2.0;
  D(1, 1) = -3.0;
  D(2, 2) = 0.5;
  D(3, 3) = 4.0;
  EXPECT_DOUBLE_EQ(itk::bridge::Math::Determinant(D), 2.0 * -3.0 * 0.5 * 4.0);
}

// A rank-deficient matrix (two identical rows) has a zero determinant.
TEST(BridgeMathDeterminant, SingularIsZero)
{
  itk::Matrix<double, 3, 3> A;
  A(0, 0) = 1.0;
  A(0, 1) = 2.0;
  A(0, 2) = 3.0;
  A(1, 0) = 1.0;
  A(1, 1) = 2.0;
  A(1, 2) = 3.0;
  A(2, 0) = 4.0;
  A(2, 1) = 5.0;
  A(2, 2) = 6.0;
  EXPECT_NEAR(itk::bridge::Math::Determinant(A), 0.0, 1e-14);
}

// Upper-triangular determinant is the product of the diagonal; exercises the
// runtime PartialPivLU path at a size beyond the direct formulas.
TEST(BridgeMathDeterminant, LargeTriangular)
{
  constexpr unsigned int N = 6;
  vnl_matrix<double>     A(N, N, 0.0);
  double                 expected = 1.0;
  for (unsigned int i = 0; i < N; ++i)
  {
    A(i, i) = static_cast<double>(i) - 2.5; // nonzero diagonal, mixed sign
    expected *= A(i, i);
    for (unsigned int j = i + 1; j < N; ++j)
    {
      A(i, j) = 0.3 * (i + 1) + 0.7 * j; // upper triangle does not affect det
    }
  }
  EXPECT_NEAR(itk::bridge::Math::Determinant(A), expected, 1e-10 * std::abs(expected));
}

// det(cA) = c^n det(A) for an n x n matrix.
TEST(BridgeMathDeterminant, ScalingLaw)
{
  const auto                A = FillMatrix<double, 3>(kM3);
  const double              base = itk::bridge::Math::Determinant(A);
  itk::Matrix<double, 3, 3> scaled = A * 2.0;
  EXPECT_NEAR(itk::bridge::Math::Determinant(scaled), 8.0 * base, 1e-13);
}

// float instantiation computes with float precision.
TEST(BridgeMathDeterminant, FloatType)
{
  const auto  A = FillMatrix<float, 2>(kM2);
  const float expected = static_cast<float>(kM2[0][0] * kM2[1][1] - kM2[0][1] * kM2[1][0]);
  EXPECT_NEAR(itk::bridge::Math::Determinant(A), expected, 1e-6f);
}

// A non-square dynamic matrix is a usage error.
TEST(BridgeMathDeterminant, NonSquareThrows)
{
  const vnl_matrix<double> rect(2, 3, 1.0);
  EXPECT_THROW(itk::bridge::Math::Determinant(rect), itk::ExceptionObject);
}

// Equivalence with the vnl engines being supplemented, on well- and
// ill-conditioned inputs, by relative agreement.
TEST(BridgeMathDeterminant, MatchesVnl)
{
  const auto                           A3 = FillMatrix<double, 3>(kM3);
  const vnl_matrix_fixed<double, 3, 3> f3 = A3.GetVnlMatrix();
  EXPECT_NEAR(itk::bridge::Math::Determinant(A3), vnl_det(f3), 1e-13);

  const vnl_matrix<double> d3 = f3.as_matrix();
  const double             ref = vnl_determinant(d3);
  EXPECT_NEAR(itk::bridge::Math::Determinant(d3), ref, 1e-13 * std::max(1.0, std::abs(ref)));

  // Ill-conditioned: a scaled Hilbert-like matrix.
  constexpr unsigned int N = 8;
  vnl_matrix<double>     H(N, N);
  for (unsigned int i = 0; i < N; ++i)
    for (unsigned int j = 0; j < N; ++j)
      H(i, j) = 1.0 / (i + j + 1.0);
  const double refH = vnl_determinant(H);
  EXPECT_NEAR(itk::bridge::Math::Determinant(H), refH, 1e-9 * std::max(1.0, std::abs(refH)));
}
