/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCoxDeBoorBSplineKernelFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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

  return EXIT_SUCCESS;
}
