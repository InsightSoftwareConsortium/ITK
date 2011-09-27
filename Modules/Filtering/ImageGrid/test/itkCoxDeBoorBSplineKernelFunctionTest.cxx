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
#include "itkCoxDeBoorBSplineKernelFunction.h"

/**
 * In this test, we check to see that the coefficients that are
 * derived using the Cox-DeBoor recursion algorithm
 */
int itkCoxDeBoorBSplineKernelFunctionTest( int argc, char * argv [] )
{
  if ( argc < 1 )
    {
    std::cerr << "Usage: " << argv[0] << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::CoxDeBoorBSplineKernelFunction<3> KernelType;
  KernelType::Pointer kernel = KernelType::New();
  KernelType::MatrixType coefficients;

  /**
   * order 0
   */
  KernelType::MatrixType trueCoefficientsOrder0(1, 1);
  trueCoefficientsOrder0[0][0] = 1.0;

  kernel->SetSplineOrder( 0 );
  coefficients = kernel->GetShapeFunctions();
  if( coefficients != trueCoefficientsOrder0 )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Spline order = " << kernel->GetSplineOrder() << std::endl;
  std::cout << "Shape functions = " << kernel->GetShapeFunctions() << std::endl;
  std::cout << "Shape functions [0,1) = "
    << kernel->GetShapeFunctionsInZeroToOneInterval() << std::endl;
  kernel->Print( std::cout, 3 );

  /**
   * order 1
   */
  KernelType::MatrixType trueCoefficientsOrder1(1, 2);
  trueCoefficientsOrder1[0][0] = -1.0;
  trueCoefficientsOrder1[0][1] = 1.0;

  kernel->SetSplineOrder( 1 );
  coefficients = kernel->GetShapeFunctions();
  if( coefficients != trueCoefficientsOrder1 )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Spline order = " << kernel->GetSplineOrder() << std::endl;
  std::cout << "Shape functions = " << kernel->GetShapeFunctions() << std::endl;
  std::cout << "Shape functions [0,1) = "
    << kernel->GetShapeFunctionsInZeroToOneInterval() << std::endl;
  kernel->Print( std::cout, 3 );

  /**
   * order 2
   */
  KernelType::MatrixType trueCoefficientsOrder2(2, 3);
  trueCoefficientsOrder2[0][0] = -1.0;
  trueCoefficientsOrder2[0][1] = 0.0;
  trueCoefficientsOrder2[0][2] = 0.75;
  trueCoefficientsOrder2[1][0] = 0.5;
  trueCoefficientsOrder2[1][1] = -1.5;
  trueCoefficientsOrder2[1][2] = 1.125;

  kernel->SetSplineOrder( 2 );
  coefficients = kernel->GetShapeFunctions();
  if( coefficients != trueCoefficientsOrder2 )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Spline order = " << kernel->GetSplineOrder() << std::endl;
  std::cout << "Shape functions = " << kernel->GetShapeFunctions() << std::endl;
  std::cout << "Shape functions [0,1) = "
    << kernel->GetShapeFunctionsInZeroToOneInterval() << std::endl;
  kernel->Print( std::cout, 3 );

  /**
   * order 3
   */
  KernelType::MatrixType trueCoefficientsOrder3(2, 4);
  trueCoefficientsOrder3[0][0] = 0.5;
  trueCoefficientsOrder3[0][1] = -1.0;
  trueCoefficientsOrder3[0][2] = 0.0;
  trueCoefficientsOrder3[0][3] = 2.0/3.0;
  trueCoefficientsOrder3[1][0] = -1.0/6.0;
  trueCoefficientsOrder3[1][1] = 1.0;
  trueCoefficientsOrder3[1][2] = -2.0;
  trueCoefficientsOrder3[1][3] = 4.0/3.0;

  kernel->SetSplineOrder( 3 );
  coefficients = kernel->GetShapeFunctions();
  if( coefficients != trueCoefficientsOrder3 )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Spline order = " << kernel->GetSplineOrder() << std::endl;
  std::cout << "Shape functions = " << kernel->GetShapeFunctions() << std::endl;
  std::cout << "Shape functions [0,1) = "
    << kernel->GetShapeFunctionsInZeroToOneInterval() << std::endl;
  kernel->Print( std::cout, 3 );

  return EXIT_SUCCESS;
}
