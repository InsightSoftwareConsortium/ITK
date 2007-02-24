/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTDistributionTest.cxx
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

#include "itkTDistribution.h"

int itkTDistributionTest(int, char* [] ) 
{
  std::cout << "itkTDistribution Test \n \n"; 
  
  typedef itk::Statistics::TDistribution DistributionType;

  DistributionType::Pointer distributionFunction = DistributionType::New();


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

    diff = fabs(value - expected1[i+5]);

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

    diff = fabs(value - double(i));

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

    diff = fabs(value - expected11[i+5]);

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

    diff = fabs(value - double(i));

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

    diff = fabs(value - expected11[i+5]);

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

    diff = fabs(value - double(i));

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

    value = distributionFunction->EvaluateCDF( x, (long)params[0] );

    diff = fabs(value - expected11[i+5]);

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

    value = distributionFunction->EvaluateInverseCDF( expected11[i+5],
                                                      (long) params[0] );

    diff = fabs(value - double(i));

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
        
  return status;
}
