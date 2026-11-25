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

#include "itkTDistribution.h"
#include "itkGTest.h"
#include "itkMath.h"
#include <cmath>

namespace
{

using DistributionType = itk::Statistics::TDistribution;

void
VerifyCDF(const DistributionType::Pointer & distributionFunction,
          const double *                    expected,
          const int                         numEntries,
          const double                      xStart,
          const double                      xStep,
          const double                      tol)
{
  for (int i = 0; i < numEntries; ++i)
  {
    const auto   x = static_cast<double>(xStart + xStep * i);
    const double value = distributionFunction->EvaluateCDF(x);
    const double diff = itk::Math::Absolute(value - expected[i]);
    EXPECT_LT(diff, tol) << "CDF mismatch at x=" << x;
  }
}

void
VerifyInverseCDF(const DistributionType::Pointer & distributionFunction,
                 const double *                    expected,
                 const int                         numEntries,
                 const double                      xStart,
                 const double                      xStep,
                 const double                      tol)
{
  for (int i = 0; i < numEntries; ++i)
  {
    const double value = distributionFunction->EvaluateInverseCDF(expected[i]);
    const double expectedX = xStart + xStep * i;
    const double diff = itk::Math::Absolute(value - expectedX);
    EXPECT_LT(diff, tol) << "Inverse CDF mismatch at expected[" << i << "]";
  }
}

// Student-t CDF at x = -5:1:5 for the degrees of freedom named in each test.
constexpr double expected1[]{ 6.283295818900114e-002, 7.797913037736926e-002, 1.024163823495667e-001,
                              1.475836176504333e-001, 2.500000000000000e-001, 5.000000000000000e-001,
                              7.500000000000000e-001, 8.524163823495667e-001, 8.975836176504333e-001,
                              9.220208696226308e-001, 9.371670418109989e-001 };

constexpr double expected11[]{ 2.012649090622596e-004, 1.043096783487477e-003, 6.039919735960683e-003,
                               3.540197753401686e-002, 1.694003480981013e-001, 5.000000000000000e-001,
                               8.305996519018988e-001, 9.645980224659831e-001, 9.939600802640394e-001,
                               9.989569032165125e-001, 9.997987350909378e-001 };

} // namespace


TEST(TDistribution, BasicObjectProperties)
{
  auto distributionFunction = DistributionType::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(distributionFunction, TDistribution, ProbabilityDistribution);
  EXPECT_TRUE(distributionFunction->HasMean());
  EXPECT_EQ(static_cast<itk::SizeValueType>(1), distributionFunction->GetNumberOfParameters());
}


TEST(TDistribution, CDF1DegreeOfFreedom)
{
  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(1);

  VerifyCDF(distributionFunction, expected1, 11, -5.0, 1.0, 1e-14);
  VerifyInverseCDF(distributionFunction, expected1, 11, -5.0, 1.0, 1e-10);
}


TEST(TDistribution, CDF11DegreesOfFreedom)
{
  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(11);

  VerifyCDF(distributionFunction, expected11, 11, -5.0, 1.0, 1e-14);
  VerifyInverseCDF(distributionFunction, expected11, 11, -5.0, 1.0, 1e-12);
}


TEST(TDistribution, ParameterVectorAPI)
{
  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(1); // clear settings

  DistributionType::ParametersType params(1);
  params[0] = 11.0;

  const double tol = 1e-14;
  for (int i = 0; i < 11; ++i)
  {
    const auto   x = static_cast<double>(-5 + i);
    const double value = distributionFunction->EvaluateCDF(x, params);
    EXPECT_LT(itk::Math::Absolute(value - expected11[i]), tol) << "CDF(param vector) mismatch at x=" << x;
  }

  const double invTol = 1e-12;
  for (int i = 0; i < 11; ++i)
  {
    const double value = distributionFunction->EvaluateInverseCDF(expected11[i], params);
    EXPECT_LT(itk::Math::Absolute(value - static_cast<double>(-5 + i)), invTol)
      << "InverseCDF(param vector) mismatch at i=" << i;
  }
}


TEST(TDistribution, SeparateParameterAPI)
{
  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(1); // clear settings

  const double tol = 1e-14;
  double       last_x = NAN;
  for (int i = 0; i < 11; ++i)
  {
    const auto x = static_cast<double>(-5 + i);
    last_x = x;
    const double value = distributionFunction->EvaluateCDF(x, static_cast<itk::SizeValueType>(11));
    EXPECT_LT(itk::Math::Absolute(value - expected11[i]), tol) << "CDF(separate param) mismatch at x=" << x;
  }

  const double invTol = 1e-12;
  for (int i = 0; i < 11; ++i)
  {
    const double value = distributionFunction->EvaluateInverseCDF(expected11[i], static_cast<itk::SizeValueType>(11));
    EXPECT_LT(itk::Math::Absolute(value - static_cast<double>(-5 + i)), invTol)
      << "InverseCDF(separate param) mismatch at i=" << i;
  }

  // The parameter-vector and separate-dof overloads must agree for the same dof.
  constexpr itk::SizeValueType     dof{ 5 };
  DistributionType::ParametersType parameters(distributionFunction->GetNumberOfParameters());
  parameters[0] = 5.0;

  EXPECT_DOUBLE_EQ(distributionFunction->PDF(last_x, parameters), distributionFunction->PDF(last_x, dof));
  EXPECT_DOUBLE_EQ(distributionFunction->EvaluatePDF(last_x, parameters), distributionFunction->PDF(last_x, dof));
  EXPECT_DOUBLE_EQ(distributionFunction->CDF(last_x, parameters), distributionFunction->CDF(last_x, dof));
  EXPECT_DOUBLE_EQ(distributionFunction->EvaluateCDF(last_x, parameters), distributionFunction->CDF(last_x, dof));
  // The symmetric Student-t has median 0.
  EXPECT_NEAR(distributionFunction->InverseCDF(0.5, dof), 0.0, 1e-10);
}


TEST(TDistribution, WrongParametersThrow)
{
  auto distributionFunction = DistributionType::New();

  const unsigned int               wrongNumberOfParameters = distributionFunction->GetNumberOfParameters() * 42;
  DistributionType::ParametersType wrongParameters(wrongNumberOfParameters, 1.0);

  distributionFunction->SetParameters(wrongParameters);

  const double x = 1.0;

  EXPECT_THROW(distributionFunction->HasVariance(), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->GetVariance(), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->GetDegreesOfFreedom(), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->PDF(x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluatePDF(x), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluatePDF(x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->CDF(x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateCDF(x), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateCDF(x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->InverseCDF(x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateInverseCDF(x), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateInverseCDF(x, wrongParameters), itk::ExceptionObject);
}


TEST(TDistribution, SetGetDegreesOfFreedom)
{
  auto distributionFunction = DistributionType::New();

  constexpr itk::SizeValueType newdof{ 17 };
  distributionFunction->SetDegreesOfFreedom(newdof);
  EXPECT_EQ(newdof, distributionFunction->GetDegreesOfFreedom());

  distributionFunction->InverseCDF(-1.0, static_cast<itk::SizeValueType>(5));

  // Drive the parameter-resize branch of SetDegreesOfFreedom from a mis-sized vector.
  DistributionType::ParametersType parameters2(2);
  parameters2[0] = 3.0;
  distributionFunction->SetParameters(parameters2);
  distributionFunction->SetDegreesOfFreedom(16);
  EXPECT_EQ(static_cast<itk::SizeValueType>(16), distributionFunction->GetDegreesOfFreedom());

  const DistributionType::ParametersType parameters0(0);
  distributionFunction->SetParameters(parameters0);
  distributionFunction->Print(std::cout);
}


TEST(TDistribution, VarianceSemantics)
{
  auto distributionFunction = DistributionType::New();

  // Variance is undefined for dof <= 2: HasVariance() false, GetVariance() NaN.
  for (const itk::SizeValueType lowDof : { 1, 2 })
  {
    distributionFunction->SetDegreesOfFreedom(lowDof);
    EXPECT_FALSE(distributionFunction->HasVariance());
    EXPECT_TRUE(std::isnan(distributionFunction->GetVariance()));
  }

  // For dof > 2 the variance is dof / (dof - 2).
  distributionFunction->SetDegreesOfFreedom(5);
  EXPECT_TRUE(distributionFunction->HasVariance());
  EXPECT_DOUBLE_EQ(5.0 / 3.0, distributionFunction->GetVariance());
}
