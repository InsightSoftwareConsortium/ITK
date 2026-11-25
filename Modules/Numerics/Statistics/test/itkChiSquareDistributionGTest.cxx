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

#include "itkChiSquareDistribution.h"
#include "itkGTest.h"
#include "itkMath.h"

namespace
{

using DistributionType = itk::Statistics::ChiSquareDistribution;

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

    std::cout << "Chi-Square cdf at ";
    std::cout.width(2);
    std::cout << x << " with ";
    std::cout.width(2);
    std::cout << distributionFunction->GetDegreesOfFreedom() << " dof = ";
    std::cout.width(20);
    std::cout << value << ", expected = ";
    std::cout.width(20);
    std::cout << expected[i] << ", error = ";
    std::cout.width(22);
    std::cout << diff << std::endl;

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

    std::cout << "Inverse CDF at ";
    std::cout.width(20);
    std::cout << expected[i] << " with ";
    std::cout.width(2);
    std::cout << distributionFunction->GetDegreesOfFreedom() << " dof = ";
    std::cout.width(22);
    std::cout << value << ", expected = ";
    std::cout.width(22);
    std::cout << expectedX << ", error = ";
    std::cout.width(22);
    std::cout << diff << std::endl;

    EXPECT_LT(diff, tol) << "Inverse CDF mismatch at expected[" << i << "]";
  }
}

} // namespace


TEST(ChiSquareDistribution, BasicObjectProperties)
{
  auto distributionFunction = DistributionType::New();

  std::cout << "GetNameOfClass() = " << distributionFunction->GetNameOfClass() << std::endl;
  std::cout << "HasMean()        = " << distributionFunction->HasMean() << std::endl;
  std::cout << "HasVariance()    = " << distributionFunction->HasVariance() << std::endl;
  std::cout << "Number of parameters = " << distributionFunction->GetNumberOfParameters() << std::endl;

  distributionFunction->Print(std::cout);
}


TEST(ChiSquareDistribution, CDF1DegreeOfFreedom)
{
  // expected values for Chi-Square cdf with 1 degree of freedom at values of 0:1:5
  constexpr double expected1[]{ 0,
                                6.826894921370859e-001,
                                8.427007929497149e-001,
                                9.167354833364458e-001,
                                9.544997361036416e-001,
                                9.746526813225318e-001 };

  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(1);

  std::cout << "Testing distribution with 1 degree of freedom" << std::endl;

  std::cout << "Chi-Square CDF" << std::endl;
  VerifyCDF(distributionFunction, expected1, 6, 0.0, 1.0, 1e-14);

  std::cout << "Inverse Chi-Square CDF" << std::endl;
  VerifyInverseCDF(distributionFunction, expected1, 6, 0.0, 1.0, 1e-12);
}


TEST(ChiSquareDistribution, CDF11DegreesOfFreedom)
{
  // expected values for Chi-Square cdf with 11 degrees of freedom at values of 0:2:20
  constexpr double expected11[]{ 0,
                                 1.504118282583805e-003,
                                 3.008297612122607e-002,
                                 1.266357467726155e-001,
                                 2.866961703699681e-001,
                                 4.696128489989594e-001,
                                 6.363567794831719e-001,
                                 7.670065225437422e-001,
                                 8.588691197329420e-001,
                                 9.184193863071046e-001,
                                 9.546593255659396e-001 };

  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(11);

  std::cout << "Testing distribution with 11 degrees of freedom" << std::endl;

  std::cout << "Chi-Square CDF" << std::endl;
  VerifyCDF(distributionFunction, expected11, 11, 0.0, 2.0, 1e-14);

  std::cout << "Inverse Chi-Square CDF" << std::endl;
  VerifyInverseCDF(distributionFunction, expected11, 11, 0.0, 2.0, 1e-12);
}


TEST(ChiSquareDistribution, CDF100DegreesOfFreedom)
{
  // expected values for Chi-Square cdf with 100 degrees of freedom at values of 50:20:150
  constexpr double expected100[]{ 6.953305247616148e-006, 9.845502476408603e-003, 2.468020344001694e-001,
                                  7.677952194991408e-001, 9.764876021901918e-001, 9.990960679576461e-001 };

  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(100);

  std::cout << "Testing distribution with 100 degrees of freedom" << std::endl;

  std::cout << "Chi-Square CDF" << std::endl;
  VerifyCDF(distributionFunction, expected100, 6, 50.0, 20.0, 1e-13);

  std::cout << "Inverse Chi-Square CDF" << std::endl;
  VerifyInverseCDF(distributionFunction, expected100, 6, 50.0, 20.0, 1e-8);
}


TEST(ChiSquareDistribution, ParameterVectorAPI)
{
  constexpr double expected100[]{ 6.953305247616148e-006, 9.845502476408603e-003, 2.468020344001694e-001,
                                  7.677952194991408e-001, 9.764876021901918e-001, 9.990960679576461e-001 };

  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(1); // clear settings

  DistributionType::ParametersType params(1);
  params[0] = 100.0;

  std::cout << "Testing Chi-Square CDF (parameter vector API)" << std::endl;
  const double tol = 1e-13;
  for (int i = 0; i <= 5; ++i)
  {
    const auto   x = static_cast<double>(50 + 20 * i);
    const double value = distributionFunction->EvaluateCDF(x, params);
    const double diff = itk::Math::Absolute(value - expected100[i]);
    EXPECT_LT(diff, tol) << "CDF(param vector) mismatch at x=" << x;
  }

  std::cout << "Testing Inverse Chi-Square CDF (parameter vector API)" << std::endl;
  const double invTol = 1e-9;
  for (int i = 0; i <= 5; ++i)
  {
    const double value = distributionFunction->EvaluateInverseCDF(expected100[i], params);
    const double diff = itk::Math::Absolute(value - static_cast<double>(50 + 20 * i));
    EXPECT_LT(diff, invTol) << "InverseCDF(param vector) mismatch at i=" << i;
  }
}


TEST(ChiSquareDistribution, SeparateParameterAPI)
{
  constexpr double expected100[]{ 6.953305247616148e-006, 9.845502476408603e-003, 2.468020344001694e-001,
                                  7.677952194991408e-001, 9.764876021901918e-001, 9.990960679576461e-001 };

  auto distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(1); // clear settings

  DistributionType::ParametersType params(1);
  params[0] = 100.0;

  std::cout << "Testing Chi-Square CDF (separate parameter API)" << std::endl;
  const double tol = 1e-13;
  double       last_x = NAN;
  for (int i = 0; i <= 5; ++i)
  {
    const auto x = static_cast<double>(50 + 20 * i);
    last_x = x;
    const double value = distributionFunction->EvaluateCDF(x, static_cast<long>(params[0]));
    const double diff = itk::Math::Absolute(value - expected100[i]);
    EXPECT_LT(diff, tol) << "CDF(separate param) mismatch at x=" << x;
  }

  std::cout << "Testing Inverse Chi-Square CDF (separate parameter API)" << std::endl;
  const double invTol = 1e-8;
  for (int i = 0; i <= 5; ++i)
  {
    const double value = distributionFunction->EvaluateInverseCDF(expected100[i], static_cast<long>(params[0]));
    const double diff = itk::Math::Absolute(value - static_cast<double>(50 + 20 * i));
    EXPECT_LT(diff, invTol) << "InverseCDF(separate param) mismatch at i=" << i;
  }

  // Exercise various API entry points
  constexpr long                   dof{ 2 };
  DistributionType::ParametersType parameters(distributionFunction->GetNumberOfParameters());
  parameters[0] = 1.0;

  std::cout << "Variance() = " << distributionFunction->GetVariance() << std::endl;
  std::cout << "PDF(x,p) = " << distributionFunction->PDF(last_x, parameters) << std::endl;
  std::cout << "PDF(x,dof) = " << distributionFunction->PDF(last_x, dof) << std::endl;
  std::cout << "EvaluatePDF(x) = " << distributionFunction->EvaluatePDF(last_x) << std::endl;
  std::cout << "EvaluatePDF(x,p) = " << distributionFunction->EvaluatePDF(last_x, parameters) << std::endl;
  std::cout << "EvaluatePDF(x,dof) = " << distributionFunction->EvaluatePDF(last_x, dof) << std::endl;
  std::cout << "CDF(x,p) = " << distributionFunction->CDF(last_x, parameters) << std::endl;
  std::cout << "CDF(x,dof) = " << distributionFunction->CDF(last_x, dof) << std::endl;
  std::cout << "EvaluateCDF(x,p) = " << distributionFunction->EvaluateCDF(last_x, parameters) << std::endl;
  std::cout << "EvaluateCDF(x,dof) = " << distributionFunction->EvaluateCDF(last_x, dof) << std::endl;
  std::cout << "InverseCDF(x,p) = " << distributionFunction->InverseCDF(last_x, parameters) << std::endl;
  std::cout << "InverseCDF(x,dof) = " << distributionFunction->InverseCDF(last_x, dof) << std::endl;
  std::cout << "GetMean() = " << distributionFunction->GetMean() << std::endl;
  std::cout << "GetVariance() = " << distributionFunction->GetVariance() << std::endl;
}


TEST(ChiSquareDistribution, WrongParametersThrow)
{
  auto distributionFunction = DistributionType::New();

  const unsigned int               wrongNumberOfParameters = distributionFunction->GetNumberOfParameters() * 42;
  DistributionType::ParametersType wrongParameters(wrongNumberOfParameters, 1.0);

  distributionFunction->SetParameters(wrongParameters);

  const double last_x = 150.0;

  EXPECT_THROW(distributionFunction->GetMean(), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->GetVariance(), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->GetDegreesOfFreedom(), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->PDF(last_x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluatePDF(last_x), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluatePDF(last_x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->CDF(last_x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateCDF(last_x), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateCDF(last_x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->InverseCDF(last_x, wrongParameters), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateInverseCDF(last_x), itk::ExceptionObject);
  EXPECT_THROW(distributionFunction->EvaluateInverseCDF(last_x, wrongParameters), itk::ExceptionObject);
}


TEST(ChiSquareDistribution, SetGetDegreesOfFreedom)
{
  auto distributionFunction = DistributionType::New();

  const unsigned int               wrongNumberOfParameters = distributionFunction->GetNumberOfParameters() * 42;
  DistributionType::ParametersType wrongParameters(wrongNumberOfParameters, 1.0);

  distributionFunction->SetParameters(wrongParameters);
  constexpr unsigned long newdof{ 17 };
  distributionFunction->SetDegreesOfFreedom(newdof);
  EXPECT_EQ(newdof, distributionFunction->GetDegreesOfFreedom());

  // Exercise a negative parameter
  constexpr long dof{ 2 };
  distributionFunction->CDF(-1.0, dof);

  // Exercise print with a parameter array of zero elements.
  const DistributionType::ParametersType parameters0(0);
  distributionFunction->SetParameters(parameters0);
  distributionFunction->Print(std::cout);

  DistributionType::ParametersType parameters1(1);
  parameters1[0] = 1.18;
  distributionFunction->SetParameters(parameters1);
  EXPECT_DOUBLE_EQ(parameters1[0], distributionFunction->GetMean());
}
