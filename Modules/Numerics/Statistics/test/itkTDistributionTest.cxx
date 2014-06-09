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

#include "itkTDistribution.h"
#include "itkTestingMacros.h"
#include "itkStdStreamStateSave.h"

int itkTDistributionTest(int, char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
  itk::StdStreamStateSave coutState(std::cout);

  std::cout << "itkTDistribution Test \n \n";

  typedef itk::Statistics::TDistribution DistributionType;

  DistributionType::Pointer distributionFunction = DistributionType::New();

  std::cout << "GetNameOfClass() = " << distributionFunction->GetNameOfClass() << std::endl;
  std::cout << "HasMean()        = " << distributionFunction->HasMean() << std::endl;
  std::cout << "Number of parameters = " << distributionFunction->GetNumberOfParameters() << std::endl;

  distributionFunction->Print( std::cout );

  int i;
  double x;
  double value;
  double diff;

  int status = EXIT_SUCCESS;

  // Tolerance for the values.
  double tol;

  // expected values for Student-t cdf with 1 degree of freedom at
  // values of -5:1:5
  double expected1[] = {6.283295818900114e-002,
                        7.797913037736926e-002,
                        1.024163823495667e-001,
                        1.475836176504333e-001,
                        2.500000000000000e-001,
                        5.000000000000000e-001,
                        7.500000000000000e-001,
                        8.524163823495667e-001,
                        8.975836176504333e-001,
                        9.220208696226308e-001,
                        9.371670418109989e-001};


  std::cout << "Testing distribution with 1 degree of freedom" << std::endl;

  std::cout << "Student-t CDF" << std::endl;
  tol = 1e-15;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  distributionFunction->SetDegreesOfFreedom( 1 );
  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x );

    diff = std::fabs(value - expected1[i+5]);

    std::cout << "Student-t cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
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

  std::cout << "Inverse Student-t CDF" << std::endl;
  tol = 1e-10;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = -5; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF( expected1[i+5] );

    diff = std::fabs(value - double(i));

    std::cout << "Student-t cdf at ";
    std::cout.width(20);
    std::cout <<  expected1[i+5] << " with ";
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


  // expected values for Student-t cdf with 11 degrees of freedom at
  // values of -5:1:5
  double expected11[] = {2.012649090622596e-004,
                         1.043096783487477e-003,
                         6.039919735960683e-003,
                         3.540197753401686e-002,
                         1.694003480981013e-001,
                         5.000000000000000e-001,
                         8.305996519018988e-001,
                         9.645980224659831e-001,
                         9.939600802640394e-001,
                         9.989569032165125e-001,
                         9.997987350909378e-001};

  std::cout << "-----------------------------------------------"
            << std::endl << std::endl;
  std::cout << "Testing distribution with 11 degrees of freedom" << std::endl;

  std::cout << "Student-t CDF" << std::endl;
  tol = 1e-15;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  distributionFunction->SetDegreesOfFreedom( 11 );
  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x );

    diff = std::fabs(value - expected11[i+5]);

    std::cout << "Student-t cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout <<  distributionFunction->GetDegreesOfFreedom()
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected11[i+5]
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

  std::cout << "Inverse Student-t CDF" << std::endl;
  tol = 1e-13;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = -5; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF( expected11[i+5] );

    diff = std::fabs(value - double(i));

    std::cout << "Student-t cdf at ";
    std::cout.width(20);
    std::cout <<  expected11[i+5] << " with ";
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


  // Same test but with the parameter vector API
  std::cout << "-----------------------------------------------"
            << std::endl << std::endl;
  std::cout << "Testing distribution with 11 degrees of freedom"
            << " (parameter vector API)" << std::endl;

  std::cout << "Student-t CDF" << std::endl;
  tol = 1e-15;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;

  distributionFunction->SetDegreesOfFreedom( 1 ); // clear settings
  DistributionType::ParametersType params(1);
  params[0] = 11;

  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF( x, params );

    diff = std::fabs(value - expected11[i+5]);

    std::cout << "Student-t cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout << " 11 "
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected11[i+5]
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

  std::cout << "Inverse Student-t CDF" << std::endl;
  tol = 1e-13;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = -5; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF( expected11[i+5],params );

    diff = std::fabs(value - double(i));

    std::cout << "Student-t cdf at ";
    std::cout.width(20);
    std::cout <<  expected11[i+5] << " with ";
    std::cout.width(2);
    std::cout << " 11 "
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


  // Same test but with the separate parameter API
  std::cout << "-----------------------------------------------"
            << std::endl << std::endl;
  std::cout << "Testing distribution with 11 degrees of freedom"
            << " (separate parameter API)" << std::endl;

  std::cout << "Student-t CDF" << std::endl;
  tol = 1e-15;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;

  for (i = -5; i <= 5; ++i)
    {
    x = static_cast<double>(i);

    value = distributionFunction->EvaluateCDF(
      x, static_cast< itk::SizeValueType >(params[0]) );

    diff = std::fabs(value - expected11[i+5]);

    std::cout << "Student-t cdf at ";
    std::cout.width(2);
    std::cout <<  x << " with ";
    std::cout.width(2);
    std::cout << " 11 "
              << " degrees of freedom = ";
    std::cout.width(20);
    std::cout <<  value
              << ", expected value = ";
    std::cout.width(20);
    std::cout <<  expected11[i+5]
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

  std::cout << "Inverse Student-t CDF" << std::endl;
  tol = 1e-13;
  std::cout << "Tolerance used for test: ";
  std::cout.width(20);
  std::cout.precision(15);
  std::cout <<  tol << std::endl;
  for (i = -5; i <= 5; ++i)
    {

    value = distributionFunction->EvaluateInverseCDF(
      expected11[i+5], static_cast< itk::SizeValueType > (params[0]) );
    diff = std::fabs(value - double(i));

    std::cout << "Student-t cdf at ";
    std::cout.width(20);
    std::cout <<  expected11[i+5] << " with ";
    std::cout.width(2);
    std::cout << " 11 "
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


  DistributionType::ParametersType parameters( distributionFunction->GetNumberOfParameters() );
  parameters[0] = 5.0;
  distributionFunction->SetParameters(parameters);

  unsigned long dof = 5;

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


  const unsigned int wrongNumberOfParameters =  distributionFunction->GetNumberOfParameters() * 42;
  DistributionType::ParametersType wrongParameters( wrongNumberOfParameters );
  wrongParameters.Fill(1.0);
  std::cout << "new number of parameters = " << wrongParameters.Size() << std::endl;
  std::cout << "current number of parameters = " << distributionFunction->GetParameters().Size() << std::endl;
  distributionFunction->SetParameters( wrongParameters );
  std::cout << "new set number of parameters = " << distributionFunction->GetParameters().Size() << std::endl;

  TRY_EXPECT_NO_EXCEPTION( distributionFunction->HasMean() );
  TRY_EXPECT_EXCEPTION( distributionFunction->HasVariance() );
  TRY_EXPECT_EXCEPTION( distributionFunction->GetVariance() );
  TRY_EXPECT_EXCEPTION( distributionFunction->GetDegreesOfFreedom() );
  TRY_EXPECT_EXCEPTION( distributionFunction->PDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->CDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->InverseCDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->PDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluatePDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateCDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateCDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateInverseCDF( x ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->EvaluateInverseCDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->CDF( x, wrongParameters ) );
  TRY_EXPECT_EXCEPTION( distributionFunction->InverseCDF( x, wrongParameters ) );

  DistributionType::ParametersType parameters1( 1 );
  parameters1[0] = 3.0;

  distributionFunction->SetParameters( parameters1 );

  TRY_EXPECT_NO_EXCEPTION( distributionFunction->HasMean() );
  TRY_EXPECT_NO_EXCEPTION( distributionFunction->HasVariance() );
  TRY_EXPECT_NO_EXCEPTION( distributionFunction->GetMean() );
  TRY_EXPECT_NO_EXCEPTION( distributionFunction->GetVariance() );

  parameters1[0] = 1.5;

  distributionFunction->SetParameters( parameters1 );

  TRY_EXPECT_NO_EXCEPTION( distributionFunction->HasMean() );
  TRY_EXPECT_NO_EXCEPTION( distributionFunction->HasVariance() );
  TRY_EXPECT_NO_EXCEPTION( distributionFunction->GetMean() );
  TRY_EXPECT_NO_EXCEPTION( distributionFunction->GetVariance() );

  std::cout << "Exercise negative argument " << std::endl;
  std::cout << "InverseCDF(x,p) = " <<  distributionFunction->InverseCDF( -1.0, dof ) << std::endl;

  unsigned long newdof = 17;
  distributionFunction->SetDegreesOfFreedom( newdof );
  TEST_SET_GET_VALUE( newdof, distributionFunction->GetDegreesOfFreedom() );


  DistributionType::ParametersType parameters2( 2 );
  parameters2[0] = 3.0;
  distributionFunction->SetParameters( parameters2 );
  distributionFunction->SetDegreesOfFreedom( 16 );

  DistributionType::ParametersType parameters0( 0 );
  distributionFunction->SetParameters( parameters0 );
  distributionFunction->Print( std::cout );

  return status;
}
