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

#include "itkChiSquareDistribution.h"
#include "itkTestingMacros.h"
#include "itkStdStreamStateSave.h"

int itkChiSquareDistributionTest(int, char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  std::cout << "itkChiSquareDistribution Test \n \n";

  typedef itk::Statistics::ChiSquareDistribution DistributionType;

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
  double tol;

  // expected values for Chi-Square cdf with 1 degree of freedom at
  // values of 0:1:5
  double expected1[] = {0,
                        6.826894921370859e-001,
                        8.427007929497149e-001,
                        9.167354833364458e-001,
                        9.544997361036416e-001,
                        9.746526813225318e-001};


  std::cout << "Testing distribution with 1 degree of freedom" << std::endl;

  std::cout << "Chi-Square CDF" << std::endl;
  tol = 1e-14;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  distributionFunction->SetDegreesOfFreedom( 1 );
  for (i = 0; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x );

    diff = std::fabs(value - expected1[i]);

    std::cout << "Chi-Square cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected1[i]
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

  std::cout << "Inverse Chi-Square CDF" << std::endl;
  tol = 1e-12;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = 0; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF( expected1[i] );

    diff = std::fabs(value - double(i));

    std::cout << "Chi-Square cdf at ";
    std::cout.width(20);
    std::cout <<  expected1[i] << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
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


  // expected values for Chi-Square cdf with 11 degrees of freedom at
  // values of 0:2:20
  double expected11[] = {0,
                         1.504118282583805e-003,
                         3.008297612122607e-002,
                         1.266357467726155e-001,
                         2.866961703699681e-001,
                         4.696128489989594e-001,
                         6.363567794831719e-001,
                         7.670065225437422e-001,
                         8.588691197329420e-001,
                         9.184193863071046e-001,
                         9.546593255659396e-001};

  std::cout << "-----------------------------------------------"
            << std::endl << std::endl;
  std::cout << "Testing distribution with 11 degrees of freedom" << std::endl;

  std::cout << "Chi-Square CDF" << std::endl;
  tol = 1e-14;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  distributionFunction->SetDegreesOfFreedom( 11 );
  for (i = 0; i <= 10; ++i)
    {
    x = static_cast<double>(2*i);

    value = distributionFunction->EvaluateCDF( x );

    diff = std::fabs(value - expected11[i]);

    std::cout << "Chi-Square cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected11[i]
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

  std::cout << "Inverse Chi-Square CDF" << std::endl;
  tol = 1e-12;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = 0; i <= 10; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF( expected11[i] );

    diff = std::fabs(value - double(2*i));

    std::cout << "Chi-Square cdf at ";
    std::cout.width(20);
    std::cout <<  expected11[i] << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  double(2*i)
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


  // expected values for Chi-Square cdf with 100 degrees of freedom at
  // values of 50:20:150
  double expected100[] = {6.953305247616148e-006,
                          9.845502476408603e-003,
                          2.468020344001694e-001,
                          7.677952194991408e-001,
                          9.764876021901918e-001,
                          9.990960679576461e-001};

  std::cout << "-----------------------------------------------"
            << std::endl << std::endl;
  std::cout << "Testing distribution with 100 degrees of freedom" << std::endl;

  std::cout << "Chi-Square CDF" << std::endl;
  tol = 1e-13;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  distributionFunction->SetDegreesOfFreedom( 100 );
  for (i = 0; i <= 5; ++i)
    {
    x = static_cast<double>(50+20*i);

    value = distributionFunction->EvaluateCDF( x );

    diff = std::fabs(value - expected100[i]);

    std::cout << "Chi-Square cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected100[i]
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

  std::cout << "Inverse Chi-Square CDF" << std::endl;
  tol = 1e-8;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = 0; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF( expected100[i] );

    diff = std::fabs(value - double(50+20*i));

    std::cout << "Chi-Square cdf at ";
    std::cout.width(20);
    std::cout <<  expected100[i] << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  double(50+20*i)
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


  std::cout << "-----------------------------------------------"
            << std::endl << std::endl;
  std::cout << "Testing distribution with 100 degrees of freedom" << std::endl;

  std::cout << "Chi-Square CDF (parameter vector API)" << std::endl;
  tol = 1e-13;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  distributionFunction->SetDegreesOfFreedom( 1 ); // clear settings

  DistributionType::ParametersType params(1);
  params[0] = 100.0;

  for (i = 0; i <= 5; ++i)
    {
    x = static_cast<double>(50+20*i);

    value = distributionFunction->EvaluateCDF( x, params );

    diff = std::fabs(value - expected100[i]);

    std::cout << "Chi-Square cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout << " 100 "
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected100[i]
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

  std::cout << "Inverse Chi-Square CDF (parameter vector API)" << std::endl;
  tol = 1e-9;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = 0; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF( expected100[i], params );

    diff = std::fabs(value - double(50+20*i));

    std::cout << "Chi-Square cdf at ";
    std::cout.width(20);
    std::cout <<  expected100[i] << " with ";
    std::cout.width(2);
    std::cout << " 100 "
              << " degrees of freedom = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  double(50+20*i)
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

  std::cout << "-----------------------------------------------"
            << std::endl << std::endl;
  std::cout << "Testing distribution with 100 degrees of freedom" << std::endl;

  std::cout << "Chi-Square CDF (separate parameter API)" << std::endl;
  tol = 1e-13;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  distributionFunction->SetDegreesOfFreedom( 1 ); // clear settings
  for (i = 0; i <= 5; ++i)
    {
    x = static_cast<double>(50+20*i);

    value = distributionFunction->EvaluateCDF( x, (long)params[0] );

    diff = std::fabs(value - expected100[i]);

    std::cout << "Chi-Square cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout << " 100 "
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected100[i]
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

  std::cout << "Inverse Chi-Square CDF (separate parameter API)" << std::endl;
  tol = 1e-8;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = 0; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF(
      expected100[i], (long)params[0] );

    diff = std::fabs(value - double(50+20*i));

    std::cout << "Chi-Square cdf at ";
    std::cout.width(20);
    std::cout <<  expected100[i] << " with ";
    std::cout.width(2);
    std::cout << " 100 "
              << " degrees of freedom = ";
    std::cout.width(22);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(22);
    std::cout <<  double(50+20*i)
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


  DistributionType::ParametersType parameters( distributionFunction->GetNumberOfParameters() );
  parameters[0] = 1.0;

  long dof = 2;

  std::cout << "Variance() = " <<  distributionFunction->GetVariance() << std::endl;
  std::cout << "PDF(x,p) = " <<  distributionFunction->PDF( x, parameters ) << std::endl;
  std::cout << "PDF(x,dof) = " <<  distributionFunction->PDF( x, dof ) << std::endl;
  std::cout << "EvaluatePDF(x) = " << distributionFunction->EvaluatePDF( x ) << std::endl;
  std::cout << "EvaluatePDF(x,p) = " << distributionFunction->EvaluatePDF( x, parameters ) << std::endl;
  std::cout << "EvaluatePDF(x,dof) = " << distributionFunction->EvaluatePDF( x, dof ) << std::endl;
  std::cout << "CDF(x,p) = " <<  distributionFunction->CDF( x, parameters ) << std::endl;
  std::cout << "CDF(x,dof) = " <<  distributionFunction->CDF( x, dof ) << std::endl;
  std::cout << "EvaluateCDF(x,p) = " << distributionFunction->EvaluateCDF( x, parameters ) << std::endl;
  std::cout << "EvaluateCDF(x,dof) = " << distributionFunction->EvaluateCDF( x, dof ) << std::endl;
  std::cout << "InverseCDF(x,p) = " <<  distributionFunction->InverseCDF( x, parameters ) << std::endl;
  std::cout << "InverseCDF(x,dof) = " <<  distributionFunction->InverseCDF( x, dof ) << std::endl;
  std::cout << "GetMean() = " <<  distributionFunction->GetMean() << std::endl;
  std::cout << "GetVariance() = " <<  distributionFunction->GetVariance() << std::endl;


  const unsigned int wrongNumberOfParameters =  distributionFunction->GetNumberOfParameters() * 42;
  DistributionType::ParametersType wrongParameters( wrongNumberOfParameters );
  wrongParameters.Fill(1.0);

  distributionFunction->SetParameters( wrongParameters );

  TRY_EXPECT_EXCEPTION( distributionFunction->GetMean() );
  TRY_EXPECT_EXCEPTION( distributionFunction->GetVariance() );
  TRY_EXPECT_EXCEPTION( distributionFunction->GetDegreesOfFreedom() );
  TRY_EXPECT_EXCEPTION( distributionFunction->PDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->CDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateCDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateCDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->InverseCDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateInverseCDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateInverseCDF( x, wrongParameters ) );

  distributionFunction->SetParameters( wrongParameters );
  unsigned long newdof = 17;
  distributionFunction->SetDegreesOfFreedom( newdof );
  TEST_SET_GET_VALUE( newdof, distributionFunction->GetDegreesOfFreedom() );

  // Exercise a negative parameter
  distributionFunction->CDF( -1.0, dof );

  // Exercise print with a parameter array of zero elements.
  DistributionType::ParametersType parameters0( 0 );
  distributionFunction->SetParameters( parameters0 );
  distributionFunction->Print( std::cout );

  DistributionType::ParametersType parameters1( 1 );
  parameters1[0] = 1.18;
  distributionFunction->SetParameters( parameters1 );
  TEST_SET_GET_VALUE( parameters1[0], distributionFunction->GetMean() );

  return status;
}
