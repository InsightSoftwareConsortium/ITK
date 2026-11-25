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
#include "itkMatrixExponential.h"
#include "itkMath.h"
#include <gtest/gtest.h>
#include <cmath>


// exp(0) == Identity
TEST(MatrixExponential, ZeroIsIdentity)
{
  const itk::Matrix<double, 3, 3> zero{}; // value-initialized to zeros
  const auto                      result = itk::Math::MatrixExponential(zero);
  for (unsigned int i = 0; i < 3; ++i)
  {
    for (unsigned int j = 0; j < 3; ++j)
    {
      EXPECT_NEAR(result(i, j), (i == j) ? 1.0 : 0.0, 1e-12);
    }
  }
}


// exp(diag(a,b)) == diag(e^a, e^b)
TEST(MatrixExponential, Diagonal)
{
  itk::Matrix<double, 2, 2> d{};
  d(0, 0) = 1.5;
  d(1, 1) = -0.7;
  const auto result = itk::Math::MatrixExponential(d);
  EXPECT_NEAR(result(0, 0), std::exp(1.5), 1e-12);
  EXPECT_NEAR(result(1, 1), std::exp(-0.7), 1e-12);
  EXPECT_NEAR(result(0, 1), 0.0, 1e-12);
  EXPECT_NEAR(result(1, 0), 0.0, 1e-12);
}


// exp([[0,-t],[t,0]]) == [[cos t,-sin t],[sin t,cos t]]
TEST(MatrixExponential, RotationGenerator)
{
  const double              t = 0.9;
  itk::Matrix<double, 2, 2> g{};
  g(0, 1) = -t;
  g(1, 0) = t;
  const auto result = itk::Math::MatrixExponential(g);
  EXPECT_NEAR(result(0, 0), std::cos(t), 1e-12);
  EXPECT_NEAR(result(0, 1), -std::sin(t), 1e-12);
  EXPECT_NEAR(result(1, 0), std::sin(t), 1e-12);
  EXPECT_NEAR(result(1, 1), std::cos(t), 1e-12);
}


// exp([[0,1],[0,0]]) == [[1,1],[0,1]]
TEST(MatrixExponential, Nilpotent)
{
  itk::Matrix<double, 2, 2> n{};
  n(0, 1) = 1.0;
  const auto result = itk::Math::MatrixExponential(n);
  EXPECT_NEAR(result(0, 0), 1.0, 1e-14);
  EXPECT_NEAR(result(0, 1), 1.0, 1e-14);
  EXPECT_NEAR(result(1, 0), 0.0, 1e-14);
  EXPECT_NEAR(result(1, 1), 1.0, 1e-14);
}


// Large-norm input (1-norm 6 > Eigen's scaling threshold ~5.37) exercises the
// scaling-and-squaring path: exp([[0,-t],[t,0]]) == [[cos t,-sin t],[sin t,cos t]]
TEST(MatrixExponential, LargeNormScalingAndSquaring)
{
  const double              t = 6.0;
  itk::Matrix<double, 2, 2> g{};
  g(0, 1) = -t;
  g(1, 0) = t;
  const auto result = itk::Math::MatrixExponential(g);
  EXPECT_NEAR(result(0, 0), std::cos(t), 1e-12);
  EXPECT_NEAR(result(0, 1), -std::sin(t), 1e-12);
  EXPECT_NEAR(result(1, 0), std::sin(t), 1e-12);
  EXPECT_NEAR(result(1, 1), std::cos(t), 1e-12);
}


// exp(A) * exp(-A) == I for a general (non-normal, large-norm) matrix
TEST(MatrixExponential, InverseProperty)
{
  itk::Matrix<double, 3, 3> a{};
  a(0, 0) = 4;
  a(0, 1) = 1;
  a(0, 2) = -2;
  a(1, 0) = 3;
  a(1, 1) = -5;
  a(1, 2) = 1;
  a(2, 0) = 0;
  a(2, 1) = 2;
  a(2, 2) = 6;
  itk::Matrix<double, 3, 3> negA{};
  for (unsigned int i = 0; i < 3; ++i)
    for (unsigned int j = 0; j < 3; ++j)
      negA(i, j) = -a(i, j);

  const auto prod = itk::Math::MatrixExponential(a) * itk::Math::MatrixExponential(negA);
  for (unsigned int i = 0; i < 3; ++i)
    for (unsigned int j = 0; j < 3; ++j)
      EXPECT_NEAR(prod(i, j), (i == j) ? 1.0 : 0.0, 1e-10);
}


// Canonical vnl_matrix_exp coverage: exponential of a cross-product
// (skew-symmetric) matrix equals the corresponding rotation matrix.
// Baseline values taken verbatim from the original VXL test_matrix_exp.cxx.
TEST(MatrixExponential, SkewSymmetricEqualsRotation)
{
  // v = (1.233, -0.572, 0.777); [v]_x = [[0,-z,y],[z,0,-x],[-y,x,0]]
  const double              x = 1.233, y = -0.572, z = 0.777;
  itk::Matrix<double, 3, 3> cross{};
  cross(0, 0) = 0;
  cross(0, 1) = -z;
  cross(0, 2) = y;
  cross(1, 0) = z;
  cross(1, 1) = 0;
  cross(1, 2) = -x;
  cross(2, 0) = -y;
  cross(2, 1) = x;
  cross(2, 2) = 0;

  const auto expX = itk::Math::MatrixExponential(cross);
  EXPECT_NEAR(expX(0, 0), 0.6221833130, 1e-10);
  EXPECT_NEAR(expX(0, 1), -0.7825192869, 1e-10);
  EXPECT_NEAR(expX(1, 1), 0.1379544126, 1e-10);
  EXPECT_NEAR(expX(2, 2), 0.2501918781, 1e-10);

  // exp of a skew-symmetric matrix is a proper rotation: orthogonal, det +1.
  const auto shouldBeIdentity = expX * itk::Matrix<double, 3, 3>(expX.GetTranspose());
  for (unsigned int i = 0; i < 3; ++i)
    for (unsigned int j = 0; j < 3; ++j)
      EXPECT_NEAR(shouldBeIdentity(i, j), (i == j) ? 1.0 : 0.0, 1e-10);
}


// float scalar type is supported
TEST(MatrixExponential, FloatScalarType)
{
  const float              t = 0.5f;
  itk::Matrix<float, 2, 2> g{};
  g(0, 1) = -t;
  g(1, 0) = t;
  const auto result = itk::Math::MatrixExponential(g);
  EXPECT_NEAR(result(0, 0), std::cos(t), 1e-5f);
  EXPECT_NEAR(result(1, 0), std::sin(t), 1e-5f);
}


// vnl_matrix_fixed overload (the type returned by itk::Matrix::GetVnlMatrix)
TEST(MatrixExponential, VnlMatrixFixedOverload)
{
  vnl_matrix_fixed<double, 2, 2> g(0.0);
  g(0, 1) = -0.9;
  g(1, 0) = 0.9;
  const auto result = itk::Math::MatrixExponential(g);
  EXPECT_NEAR(result(0, 0), std::cos(0.9), 1e-12);
  EXPECT_NEAR(result(1, 0), std::sin(0.9), 1e-12);
}


// dynamically-sized vnl_matrix overload
TEST(MatrixExponential, VnlMatrixDynamicOverload)
{
  vnl_matrix<double> n(2, 2, 0.0);
  n(0, 1) = 1.0;
  const auto result = itk::Math::MatrixExponential(n);
  EXPECT_EQ(result.rows(), 2u);
  EXPECT_NEAR(result(0, 0), 1.0, 1e-14);
  EXPECT_NEAR(result(0, 1), 1.0, 1e-14);
  EXPECT_NEAR(result(1, 1), 1.0, 1e-14);
}
