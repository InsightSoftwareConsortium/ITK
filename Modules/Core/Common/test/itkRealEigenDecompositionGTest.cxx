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
#include "itkRealEigenDecomposition.h"
#include "itkConfigure.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_TEST // exercise the deprecated vnl class for equivalence
#  include "vnl/algo/vnl_real_eigensystem.h"
#endif
#include <gtest/gtest.h>
#include <algorithm>
#include <complex>
#include <limits>
#include <string>
#include <vector>

namespace
{

// Largest residual of the eigendecomposition identity A v_i == lambda_i v_i,
// over every eigenpair. Sign/phase-invariant, so it is the right thing to
// assert against (individual eigenvector signs are arbitrary).
template <typename TReal>
TReal
EigenResidual(const vnl_matrix<TReal> & A, const itk::RealEigenDecomposition<TReal> & eig)
{
  using Complex = std::complex<TReal>;
  const auto &       lambda = eig.GetEigenvalues();
  const auto &       V = eig.GetEigenvectors();
  const unsigned int n = A.rows();

  TReal worst = 0;
  for (unsigned int j = 0; j < n; ++j)
  {
    for (unsigned int r = 0; r < n; ++r)
    {
      Complex av{ 0, 0 };
      for (unsigned int k = 0; k < n; ++k)
      {
        av += Complex(A(r, k), 0) * V(k, j);
      }
      worst = std::max(worst, std::abs(av - lambda[j] * V(r, j)));
    }
  }
  return worst;
}

} // namespace

// Scavenged from VXL test_real_eigensystem::test_6x6 (real spectrum).
TEST(RealEigenDecomposition, SymmetricInputRealSpectrum)
{
  const double Sdata[36] = {
    30.0000, -3.4273, 13.9254, 13.7049, -2.4446, 20.2380, -3.4273, 13.7049, -2.4446, 1.3659,  3.6702,  -0.2282,
    13.9254, -2.4446, 20.2380, 3.6702,  -0.2282, 28.6779, 13.7049, 1.3659,  3.6702,  12.5273, -1.6045, 3.9419,
    -2.4446, 3.6702,  -0.2282, -1.6045, 3.9419,  2.5821,  20.2380, -0.2282, 28.6779, 3.9419,  2.5821,  44.0636,
  };
  const vnl_matrix<double>                  S(Sdata, 6, 6);
  const itk::RealEigenDecomposition<double> eig(S);

  for (unsigned int i = 0; i < 6; ++i)
  {
    EXPECT_LT(std::abs(std::imag(eig.GetEigenvalues()[i])), 1e-12);
  }
  EXPECT_LT(EigenResidual(S, eig), 1e-12);
}

// Scavenged from VXL test_real_eigensystem::test_4x4 (general/unsympathetic).
TEST(RealEigenDecomposition, GeneralMatrix)
{
  const double             Xdata[16] = { 686, 526, 701, 47, 588, 91, 910, 736, 930, 653, 762, 328, 846, 415, 262, 632 };
  const vnl_matrix<double> X(Xdata, 4, 4);
  const itk::RealEigenDecomposition<double> eig(X);
  EXPECT_LT(EigenResidual(X, eig), 1e-10);
}

// A real matrix with a genuinely complex spectrum: 2-D rotation by theta has
// eigenvalues exp(+/- i theta). Exercises the complex-conjugate path.
TEST(RealEigenDecomposition, ComplexSpectrumRotation)
{
  const double       theta = 0.7;
  vnl_matrix<double> R(2, 2);
  R(0, 0) = std::cos(theta);
  R(0, 1) = -std::sin(theta);
  R(1, 0) = std::sin(theta);
  R(1, 1) = std::cos(theta);

  const itk::RealEigenDecomposition<double> eig(R);
  EXPECT_LT(EigenResidual(R, eig), 1e-13);

  double maxImag = 0;
  for (unsigned int i = 0; i < 2; ++i)
  {
    maxImag = std::max(maxImag, std::abs(std::imag(eig.GetEigenvalues()[i])));
  }
  EXPECT_NEAR(maxImag, std::sin(theta), 1e-13);
}

TEST(RealEigenDecomposition, Identity)
{
  vnl_matrix<double> I(3, 3, 0.0);
  for (unsigned int i = 0; i < 3; ++i)
  {
    I(i, i) = 1.0;
  }
  const itk::RealEigenDecomposition<double> eig(I);
  for (unsigned int i = 0; i < 3; ++i)
  {
    EXPECT_NEAR(std::real(eig.GetEigenvalues()[i]), 1.0, 1e-14);
    EXPECT_LT(std::abs(std::imag(eig.GetEigenvalues()[i])), 1e-14);
  }
}

TEST(RealEigenDecomposition, FloatInstantiation)
{
  const float                              data[4] = { 2.0f, 0.0f, 0.0f, 3.0f };
  const vnl_matrix<float>                  A(data, 2, 2);
  const itk::RealEigenDecomposition<float> eig(A);
  EXPECT_LT(EigenResidual(A, eig), 1e-5f);
}

// Non-finite input leaves the Eigen solver in a non-Success state; the wrapper
// surfaces that as an exception rather than returning a garbage decomposition.
TEST(RealEigenDecomposition, NonFiniteInputThrows)
{
  const double       nan = std::numeric_limits<double>::quiet_NaN();
  vnl_matrix<double> M(2, 2, 0.0);
  M(0, 0) = 1.0;
  M(1, 1) = 1.0;
  M(0, 1) = M(1, 0) = nan;
  try
  {
    const itk::RealEigenDecomposition<double> eig{ M };
    FAIL() << "expected an exception for non-finite input";
  }
  catch (const itk::ExceptionObject & e)
  {
    const std::string description = e.GetDescription();
    EXPECT_NE(description.find("EigenSolver"), std::string::npos) << description;
    // The message names the decoded Eigen failure mode, not a bare status code.
    EXPECT_TRUE(description.find("NoConvergence") != std::string::npos ||
                description.find("NumericalIssue") != std::string::npos ||
                description.find("InvalidInput") != std::string::npos)
      << description;
  }
}

#ifndef ITK_FUTURE_LEGACY_REMOVE
// Equivalence: the Eigen-backed solver yields the same (complex) spectrum as
// the deprecated netlib vnl_real_eigensystem. General eigenvalues are unsorted,
// so compare as multisets.
TEST(RealEigenDecomposition, EquivalenceWithVnl)
{
  const double             data[9] = { 0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 2.0 }; // spectrum {i, -i, 2}
  const vnl_matrix<double> A(data, 3, 3);

  const vnl_real_eigensystem                vnlEngine(A);
  const itk::RealEigenDecomposition<double> itkEngine(A);

  auto sorted = [](std::vector<std::complex<double>> v) {
    std::sort(v.begin(), v.end(), [](const std::complex<double> & a, const std::complex<double> & b) {
      return a.real() != b.real() ? a.real() < b.real() : a.imag() < b.imag();
    });
    return v;
  };
  std::vector<std::complex<double>> vnlEig;
  std::vector<std::complex<double>> itkEig;
  for (unsigned int i = 0; i < A.rows(); ++i)
  {
    vnlEig.push_back(vnlEngine.D(i, i));
    itkEig.push_back(itkEngine.GetEigenvalues()[i]);
  }
  vnlEig = sorted(vnlEig);
  itkEig = sorted(itkEig);
  for (unsigned int i = 0; i < itkEig.size(); ++i)
  {
    EXPECT_NEAR(vnlEig[i].real(), itkEig[i].real(), 1e-10) << "eigenvalue " << i << " real";
    EXPECT_NEAR(vnlEig[i].imag(), itkEig[i].imag(), 1e-10) << "eigenvalue " << i << " imag";
  }
}
#endif
