/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDistributionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkGaussianDistribution.h"
#include <math.h>

int itkGaussianDistributionTest(int, char* [] ) 
{
  std::cout << "itkGaussianDistribution Test \n \n"; 
  
  typedef itk::Statistics::GaussianDistribution DistributionType;

  DistributionType::Pointer distributionFunction = DistributionType::New();

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

  return status;


}



