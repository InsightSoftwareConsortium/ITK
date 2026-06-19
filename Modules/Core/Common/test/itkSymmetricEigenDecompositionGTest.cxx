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
#include "itkSymmetricEigenDecomposition.h"
#include "itkConfigure.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST // exercise the deprecated vnl class for equivalence
#  include "vnl/algo/vnl_symmetric_eigensystem.h"
#endif
#include <gtest/gtest.h>
#include <limits>
#include <string>

namespace
{

template <typename T>
T
EigenResidual(const vnl_matrix<T> & A, const itk::SymmetricEigenDecomposition<T> & eig)
{
  vnl_matrix<T> D(A.rows(), A.cols(), T{ 0 });
  for (unsigned int i = 0; i < A.rows(); ++i)
  {
    D(i, i) = eig.get_eigenvalue(i);
  }
  return (A * eig.V - eig.V * D).fro_norm();
}

vnl_matrix<double>
MakeSymmetric()
{
  double data[9] = { 4.0, 1.0, -2.0, 1.0, 2.0, 0.5, -2.0, 0.5, 3.0 };
  return vnl_matrix<double>(data, 3, 3);
}

} // namespace

// Scavenged coverage: eigenvalues ascending, A V == V D, V orthonormal.
TEST(SymmetricEigenDecomposition, DecompositionIdentity)
{
  const vnl_matrix<double>                       A = MakeSymmetric();
  const itk::SymmetricEigenDecomposition<double> eig(A);

  for (unsigned int i = 1; i < 3; ++i)
  {
    EXPECT_LE(eig.get_eigenvalue(i - 1), eig.get_eigenvalue(i) + 1e-12);
  }
  EXPECT_LT(EigenResidual(A, eig), 1e-12);

  const vnl_matrix<double> vtv = eig.V.transpose() * eig.V;
  for (unsigned int r = 0; r < 3; ++r)
  {
    for (unsigned int c = 0; c < 3; ++c)
    {
      EXPECT_NEAR(vtv(r, c), (r == c) ? 1.0 : 0.0, 1e-12);
    }
  }
}

// itk::SymmetricEigenDecomposition and the legacy vnl class agree on eigenvalues
// exactly and on eigenvectors up to per-column sign.
#ifndef ITK_FUTURE_LEGACY_REMOVE
TEST(SymmetricEigenDecomposition, EquivalenceWithVnl)
{
  const vnl_matrix<double>                       A = MakeSymmetric();
  const itk::SymmetricEigenDecomposition<double> eigNew(A);
  const vnl_symmetric_eigensystem<double>        eigVnl(A);

  for (unsigned int i = 0; i < 3; ++i)
  {
    EXPECT_NEAR(eigNew.get_eigenvalue(i), eigVnl.get_eigenvalue(i), 1e-12);

    // Eigenvector columns match up to sign: |new . vnl| == 1.
    const double dot = dot_product(eigNew.get_eigenvector(i), eigVnl.get_eigenvector(i));
    EXPECT_NEAR(std::abs(dot), 1.0, 1e-12);
  }
}
#endif // ITK_FUTURE_LEGACY_REMOVE

// The deterministic sign convention: each eigenvector column's
// largest-magnitude entry is positive. This makes the decomposition
// reproducible across solver/SIMD differences.
TEST(SymmetricEigenDecomposition, CanonicalSignConvention)
{
  const vnl_matrix<double>                       A = MakeSymmetric();
  const itk::SymmetricEigenDecomposition<double> eig(A);

  for (unsigned int j = 0; j < 3; ++j)
  {
    const vnl_vector<double> col = eig.get_eigenvector(j);
    unsigned int             pivot = 0;
    for (unsigned int i = 1; i < 3; ++i)
    {
      if (std::abs(col[i]) > std::abs(col[pivot]))
      {
        pivot = i;
      }
    }
    EXPECT_GT(col[pivot], 0.0);
  }
}

// canonicalizeSigns=false keeps the solver's raw signs but stays a valid
// decomposition; the default (true) enforces largest-magnitude-positive.
TEST(SymmetricEigenDecomposition, CanonicalizationIsOptOut)
{
  const vnl_matrix<double>                       A = MakeSymmetric();
  const itk::SymmetricEigenDecomposition<double> canon(A); // default true
  const itk::SymmetricEigenDecomposition<double> raw(A, false);

  EXPECT_LT(EigenResidual(A, canon), 1e-12);
  EXPECT_LT(EigenResidual(A, raw), 1e-12);
  for (unsigned int j = 0; j < 3; ++j)
  {
    EXPECT_NEAR(canon.get_eigenvalue(j), raw.get_eigenvalue(j), 1e-12);
    EXPECT_NEAR(std::abs(dot_product(canon.get_eigenvector(j), raw.get_eigenvector(j))), 1.0, 1e-12);
  }
}

TEST(SymmetricEigenDecomposition, NullvectorAndRecompose)
{
  const vnl_matrix<double>                       A = MakeSymmetric();
  const itk::SymmetricEigenDecomposition<double> eig(A);

  // nullvector is the smallest-eigenvalue eigenvector (column 0).
  EXPECT_NEAR(dot_product(eig.nullvector(), eig.get_eigenvector(0)), 1.0, 1e-12);

  // recompose() reconstructs A.
  EXPECT_LT((eig.recompose() - A).fro_norm(), 1e-12);
}

TEST(SymmetricEigenDecomposition, FloatInstantiation)
{
  float                                         data[4] = { 2.0f, 0.5f, 0.5f, 3.0f };
  const vnl_matrix<float>                       A(data, 2, 2);
  const itk::SymmetricEigenDecomposition<float> eig(A);
  EXPECT_LT(EigenResidual(A, eig), 1e-5f);
}

// Non-finite input leaves the Eigen solver in a non-Success state; the wrapper
// surfaces that as an exception rather than returning a garbage decomposition.
TEST(SymmetricEigenDecomposition, NonFiniteInputThrows)
{
  const double       nan = std::numeric_limits<double>::quiet_NaN();
  vnl_matrix<double> M(2, 2, 0.0);
  M(0, 0) = 1.0;
  M(1, 1) = 1.0;
  M(0, 1) = M(1, 0) = nan;
  try
  {
    const itk::SymmetricEigenDecomposition<double> eig{ M };
    FAIL() << "expected an exception for non-finite input";
  }
  catch (const itk::ExceptionObject & e)
  {
    const std::string description = e.GetDescription();
    EXPECT_NE(description.find("SelfAdjointEigenSolver"), std::string::npos) << description;
    // The message names the decoded Eigen failure mode, not a bare status code.
    EXPECT_TRUE(description.find("NoConvergence") != std::string::npos ||
                description.find("NumericalIssue") != std::string::npos ||
                description.find("InvalidInput") != std::string::npos)
      << description;
  }
}
