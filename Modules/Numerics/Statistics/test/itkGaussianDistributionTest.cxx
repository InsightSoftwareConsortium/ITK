/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkGaussianDistribution.h"
#include "itkTestingMacros.h"
#include "itkStdStreamStateSave.h"

int itkGaussianDistributionTest(int, char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
  itk::StdStreamStateSave coutState(std::cout);

  std::cout << "itkGaussianDistribution Test \n \n";

  typedef itk::Statistics::GaussianDistribution DistributionType;

  DistributionType::Pointer distributionFunction = DistributionType::New();

  std::cout << "GetNameOfClass() = " << distributionFunction->GetNameOfClass() << std::endl;
  std::cout << "HasMean()        = " << distributionFunction->HasMean() << std::endl;
  std::cout << "HasVariance()    = " << distributionFunction->HasVariance() << std::endl;
  std::cout << "Number of parameters = " << distributionFunction->GetNumberOfParameters() << std::endl;

  distributionFunction->Print( std::cout );

  int i;
  double x;
  double value;
  double diff;

  int status = EXIT_SUCCESS;

  // Tolerance for the values.
  double tol = 1e-8;
  std::cout << "Tolerance used for test: ";
  std::cout.width(22);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  std::cout << std::endl;

  // expected values for Gaussian cdf with mean 0 and variance 1 at
  // values of -5:1:5
  double expected1[] = {2.866515718791942e-007,
                        3.167124183311998e-005,
                        1.349898031630095e-003,
                        2.275013194817922e-002,
                        1.586552539314571e-001,
                        5.000000000000000e-001,
                        8.413447460685429e-001,
                        9.772498680518208e-001,
                        9.986501019683699e-001,
                        9.999683287581669e-001,
                        9.999997133484281e-001};

  std::cout << "Gaussian CDF" << std::endl;
  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x );

    diff = fabs(value - expected1[i+5]);

    std::cout << "Gaussian cdf at ";
    std::cout.width(2);
    std::cout <<  x << " = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  expected1[i+5]
              << ", error = ";
    std::cout.width(22);
    std::cout <<  diff;
    if (diff < tol)
      {
      std::cout << ", Passed." << std::endl;
      }
    else
      {
      std::cout << ", Failed." << std::endl;
      status = EXIT_FAILURE;
      }
    }
  std::cout << std::endl;


  std::cout << "Inverse Gaussian CDF" << std::endl;
  for (i = -5; i <= 5; ++i)
    {
    value = distributionFunction->EvaluateInverseCDF( expected1[i+5] );

    diff = fabs(value - double(i));

    std::cout << "Inverse Gaussian cdf at ";
    std::cout.width(22);
    std::cout <<  expected1[i+5] << " = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  double(i)
              << ", error = ";
    std::cout.width(22);
    std::cout <<  diff;
    if (diff < tol)
      {
      std::cout << ", Passed." << std::endl;
      }
    else
      {
      std::cout << ", Failed." << std::endl;
      status = EXIT_FAILURE;
      }
    }
  std::cout << std::endl;

  // do the same tests at a different mean/variance
  distributionFunction->SetMean( 5.0 );
  distributionFunction->SetVariance( 2.0 );

  std::cout << "Testing mean = " << distributionFunction->GetMean()
            << ", variance = " << distributionFunction->GetVariance()
            << std::endl;

  double  expected2[] = {7.687298972140230e-013,
                         9.830802207714426e-011,
                         7.708628950140045e-009,
                         3.715491861707074e-007,
                         1.104524849929275e-005,
                         2.034760087224798e-004,
                         2.338867490523635e-003,
                         1.694742676234465e-002,
                         7.864960352514258e-002,
                         2.397500610934768e-001,
                         5.000000000000000e-001};

  std::cout << "Gaussian CDF" << std::endl;
  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x );

    diff = fabs(value - expected2[i+5]);

    std::cout << "Gaussian cdf at ";
    std::cout.width(2);
    std::cout <<  x << " = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  expected2[i+5]
              << ", error = ";
    std::cout.width(22);
    std::cout <<  diff;
    if (diff < tol)
      {
      std::cout << ", Passed." << std::endl;
      }
    else
      {
      std::cout << ", Failed." << std::endl;
      status = EXIT_FAILURE;
      }
    }
  std::cout << std::endl;

  // same test but using the parameter vector API
  DistributionType::ParametersType params(2);
  params[0] = 5.0;
  params[1] = 2.0;

  std::cout << "Testing mean = " << params[0]
            << ", variance = " << params[1]
            << std::endl;

  distributionFunction->SetMean(0.0);    // clear settings
  distributionFunction->SetVariance(1.0); // clear settings

  double  expected3[] = {7.687298972140230e-013,
                         9.830802207714426e-011,
                         7.708628950140045e-009,
                         3.715491861707074e-007,
                         1.104524849929275e-005,
                         2.034760087224798e-004,
                         2.338867490523635e-003,
                         1.694742676234465e-002,
                         7.864960352514258e-002,
                         2.397500610934768e-001,
                         5.000000000000000e-001};

  std::cout << "Gaussian CDF (parameter vector API)" << std::endl;
  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x, params );

    diff = fabs(value - expected3[i+5]);

    std::cout << "Gaussian cdf at ";
    std::cout.width(2);
    std::cout <<  x << " = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  expected3[i+5]
              << ", error = ";
    std::cout.width(22);
    std::cout <<  diff;
    if (diff < tol)
      {
      std::cout << ", Passed." << std::endl;
      }
    else
      {
      std::cout << ", Failed." << std::endl;
      status = EXIT_FAILURE;
      }
    }
  std::cout << std::endl;

  // same test but using the separate parameters
  std::cout << "Testing mean = " << params[0]
            << ", variance = " << params[1]
            << std::endl;

  double  expected4[] = {7.687298972140230e-013,
                         9.830802207714426e-011,
                         7.708628950140045e-009,
                         3.715491861707074e-007,
                         1.104524849929275e-005,
                         2.034760087224798e-004,
                         2.338867490523635e-003,
                         1.694742676234465e-002,
                         7.864960352514258e-002,
                         2.397500610934768e-001,
                         5.000000000000000e-001};

  std::cout << "Gaussian CDF (separate parameter API)" << std::endl;
  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x, params[0], params[1] );

    diff = fabs(value - expected4[i+5]);

    std::cout << "Gaussian cdf at ";
    std::cout.width(2);
    std::cout <<  x << " = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  expected4[i+5]
              << ", error = ";
    std::cout.width(22);
    std::cout <<  diff;
    if (diff < tol)
      {
      std::cout << ", Passed." << std::endl;
      }
    else
      {
      std::cout << ", Failed." << std::endl;
      status = EXIT_FAILURE;
      }
    }
  std::cout << std::endl;

  std::cout << "Inverse Gaussian CDF" << std::endl;
  // put the parameters back
  distributionFunction->SetParameters( params );
  for (i = -5; i <= 5; ++i)
    {
    value = distributionFunction->EvaluateInverseCDF( expected2[i+5] );

    diff = fabs(value - double(i));

    std::cout << "Inverse Gaussian cdf at ";
    std::cout.width(22);
    std::cout <<  expected2[i+5] << " = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  double(i)
              << ", error = ";
    std::cout.width(22);
    std::cout <<  diff;
    if (diff < tol)
      {
      std::cout << ", Passed." << std::endl;
      }
    else
      {
      std::cout << ", Failed." << std::endl;
      status = EXIT_FAILURE;
      }
    }
  std::cout << std::endl;

  const double mean1 = 1.0;
  const double variance1 = 2.5;

  DistributionType::ParametersType parameters( distributionFunction->GetNumberOfParameters() );
  parameters[0] = mean1;
  parameters[1] = variance1;
  distributionFunction->SetParameters(parameters);
  x = .1;
  std::cout << "Parameters = " << parameters << std::endl;
  std::cout << "Variance() = " <<  distributionFunction->GetVariance() << std::endl;
  std::cout << "PDF(x,p) = " <<  distributionFunction->PDF( x, parameters ) << std::endl;
  std::cout << "EvaluatePDF(x) = " << distributionFunction->EvaluatePDF( x ) << std::endl;
  std::cout << "EvaluatePDF(x,p) = " << distributionFunction->EvaluatePDF( x, parameters ) << std::endl;
  std::cout << "EvaluatePDF(x,m,v) = " << distributionFunction->EvaluatePDF( x, mean1, variance1 ) << std::endl;
  std::cout << "CDF(x,p) = " <<  distributionFunction->CDF( x, parameters ) << std::endl;
  std::cout << "EvaluateCDF(x,p) = " << distributionFunction->EvaluateCDF( x, parameters ) << std::endl;
  std::cout << "InverseCDF(x,p) = " <<  distributionFunction->InverseCDF( x, parameters ) << std::endl;
  std::cout << "InverseCDF(x,mean,variance) = " <<  distributionFunction->InverseCDF( x, mean1, variance1 ) << std::endl;
  std::cout << "EvaluateInverseCDF(x) = " <<  distributionFunction->EvaluateInverseCDF( x ) << std::endl;
  std::cout << "EvaluateInverseCDF(x,p) = " <<  distributionFunction->EvaluateInverseCDF( x, parameters ) << std::endl;
  std::cout << "EvaluateInverseCDF(x,m,v) = " << distributionFunction->EvaluateInverseCDF( x, mean1, variance1 ) << std::endl;

  std::cout << "InverseCDF(10.0,p) = " <<  distributionFunction->InverseCDF( 10.0, parameters ) << std::endl;
  std::cout << "InverseCDF(10.0,mean,variance) = " <<  distributionFunction->InverseCDF( 10.0, mean1, variance1 ) << std::endl;
  std::cout << "EvaluateInverseCDF(10.0) = " <<  distributionFunction->EvaluateInverseCDF( 10.0 ) << std::endl;
  std::cout << "EvaluateInverseCDF(10.0,p) = " <<  distributionFunction->EvaluateInverseCDF( 10.0, parameters ) << std::endl;
  std::cout << "EvaluateInverseCDF(10.0,m,v) = " << distributionFunction->EvaluateInverseCDF( 10.0, mean1, variance1 ) << std::endl;

  std::cout << "InverseCDF(-10.0,p) = " <<  distributionFunction->InverseCDF( -10.0, parameters ) << std::endl;
  std::cout << "InverseCDF(-10.0,mean,variance) = " <<  distributionFunction->InverseCDF( -10.0, mean1, variance1 ) << std::endl;
  std::cout << "EvaluateInverseCDF(-10.0) = " <<  distributionFunction->EvaluateInverseCDF( -10.0 ) << std::endl;
  std::cout << "EvaluateInverseCDF(-10.0,p) = " <<  distributionFunction->EvaluateInverseCDF( -10.0, parameters ) << std::endl;
  std::cout << "EvaluateInverseCDF(-10.0,m,v) = " << distributionFunction->EvaluateInverseCDF( -10.0, mean1, variance1 ) << std::endl;

  const unsigned int wrongNumberOfParameters =  distributionFunction->GetNumberOfParameters() * 42;
  DistributionType::ParametersType wrongParameters( wrongNumberOfParameters );
  wrongParameters.Fill(1.0);
  distributionFunction->SetParameters( wrongParameters );

  TRY_EXPECT_EXCEPTION( distributionFunction->GetVariance() );
  TRY_EXPECT_EXCEPTION( distributionFunction->GetMean() );
  TRY_EXPECT_EXCEPTION( distributionFunction->PDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateCDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateCDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateInverseCDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateInverseCDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->CDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->InverseCDF( x, wrongParameters ) );

  distributionFunction->SetParameters( wrongParameters );
  double newMean = 17.0;
  distributionFunction->SetMean( newMean );
  TEST_SET_GET_VALUE( newMean, distributionFunction->GetMean() );

  distributionFunction->SetParameters( wrongParameters );
  double newVariance = 42.0;
  distributionFunction->SetVariance( newVariance );
  TEST_SET_GET_VALUE( newVariance, distributionFunction->GetVariance() );

  const double mean2 = 0.0;
  const double variance2 = 1.0;

  parameters[0] = mean2;
  parameters[1] = variance2;

  distributionFunction->SetParameters( parameters );

  std::cout << "Parameters = " << parameters << std::endl;
  std::cout << "Variance() = " <<  distributionFunction->GetVariance() << std::endl;
  std::cout << "PDF(x,p) = " <<  distributionFunction->PDF( x, parameters ) << std::endl;
  std::cout << "EvaluatePDF(x) = " << distributionFunction->EvaluatePDF( x ) << std::endl;
  std::cout << "EvaluatePDF(x,p) = " << distributionFunction->EvaluatePDF( x, parameters ) << std::endl;
  std::cout << "EvaluatePDF(x,m,v) = " << distributionFunction->EvaluatePDF( x, mean2, variance2 ) << std::endl;
  std::cout << "CDF(x,p) = " <<  distributionFunction->CDF( x, parameters ) << std::endl;
  std::cout << "EvaluateCDF(x,p) = " << distributionFunction->EvaluateCDF( x, parameters ) << std::endl;
  std::cout << "EvaluateCDF(x,m,v) = " << distributionFunction->EvaluateCDF( x, mean2, variance2 ) << std::endl;
  std::cout << "InverseCDF(x,p) = " <<  distributionFunction->InverseCDF( x, parameters ) << std::endl;
  std::cout << "EvaluateInverseCDF(x,p) = " <<  distributionFunction->EvaluateInverseCDF( x, parameters ) << std::endl;
  std::cout << "EvaluateInverseCDF(x,m,v) = " << distributionFunction->EvaluateInverseCDF( x, mean2, variance2 ) << std::endl;

  DistributionType::ParametersType parameters0( 0 );
  distributionFunction->SetParameters( parameters0 );
  distributionFunction->Print( std::cout );

  DistributionType::ParametersType parameters1( 1 );
  parameters1.Fill(1.0);
  distributionFunction->SetParameters( parameters1 );
  distributionFunction->Print( std::cout );

  return status;
}
