/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCoxDeBoorBSplineKernelFunctionTest2.cxx
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
 * In this test, we check to see that the derivative is calculated
 * correctly for spline orders 2 through 10
 */
int itkCoxDeBoorBSplineKernelFunctionTest2( int argc, char * argv [] )
{
  if ( argc < 1 )
    {
    std::cerr << "Usage: " << argv[0] << std::endl;
    return EXIT_FAILURE; 
    } 

  typedef itk::CoxDeBoorBSplineKernelFunction<3> KernelType;
  KernelType::Pointer kernel = KernelType::New();

  KernelType::Pointer kernelOrderMinus1 = KernelType::New();

  for( unsigned int order = 2; order <= 10; order++ )
    {
    kernel->SetSplineOrder( order );
    kernelOrderMinus1->SetSplineOrder( order - 1 );

    for( double t = 0.0; t < static_cast<double>( 0.5*( order+1 ) ); t += 0.1 )
      {
      KernelType::RealType derivative = kernel->EvaluateDerivative( t );
      if( vnl_math_abs( derivative - ( kernelOrderMinus1->Evaluate( t + 0.5 )
           - kernelOrderMinus1->Evaluate( t - 0.5 ) ) ) > 1e-10 )
        {
        return EXIT_FAILURE;
        }

      // try calculating the nth derivative
      for( unsigned int d = 2; d < order-1; d++ )
        {
        try
          {
          kernel->EvaluateNthDerivative( t, d );
          }
        catch(...)
          {
          return EXIT_FAILURE;
          }
        }
      }
    }

  return EXIT_SUCCESS;
}
