/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineKernelFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkBSplineKernelFunction.h"

/*
 * This test exercises the BSpline kernel function
 * of various orders.
 */
int itkBSplineKernelFunctionTest(int, char* [] )
{

// Externally generated results
const unsigned int npoints = 49;

const double x[npoints] = { 
-3, -2.875, -2.75, -2.625, -2.5, 
-2.375, -2.25, -2.125, -2, -1.875, 
-1.75, -1.625, -1.5, -1.375, -1.25, 
-1.125, -1, -0.875, -0.75, -0.625, 
-0.5, -0.375, -0.25, -0.125, 0, 
0.125, 0.25, 0.375, 0.5, 0.625, 
0.75, 0.875, 1, 1.125, 1.25, 
1.375, 1.5, 1.625, 1.75, 1.875, 
2, 2.125, 2.25, 2.375, 2.5, 
2.625, 2.75, 2.875, 3 }; 

const double b0[npoints] = { 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0.5, 1, 1, 1, 1, 
1, 1, 1, 0.5, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0 }; 

const double b1[npoints] = { 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0.125, 0.25, 0.375, 
0.5, 0.625, 0.75, 0.875, 1, 
0.875, 0.75, 0.625, 0.5, 0.375, 
0.25, 0.125, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0 }; 

const double b2[npoints] = { 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0.0078125, 0.03125, 
0.0703125, 0.125, 0.195313, 0.28125, 0.382813, 
0.5, 0.609375, 0.6875, 0.734375, 0.75, 
0.734375, 0.6875, 0.609375, 0.5, 0.382813, 
0.28125, 0.195313, 0.125, 0.0703125, 0.03125, 
0.0078125, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 
0, 0, 0, 0 }; 

const double b3[npoints] = { 
0, 0, 0, 0, 0, 
0, 0, 0, 0, 0.000325521, 
0.00260417, 0.00878906, 0.0208333, 0.0406901, 0.0703125, 
0.111654, 0.166667, 0.236003, 0.315104, 0.398112, 
0.479167, 0.552409, 0.611979, 0.652018, 0.666667, 
0.652018, 0.611979, 0.552409, 0.479167, 0.398112, 
0.315104, 0.236003, 0.166667, 0.111654, 0.0703125, 
0.0406901, 0.0208333, 0.00878906, 0.00260417, 0.000325521, 
0, 0, 0, 0, 0, 
0, 0, 0, 0 }; 


 // Testing the output of BSplineKernelFunction
#define TEST_BSPLINE_KERNEL(ORDERNUM) \
 { \
  typedef itk::BSplineKernelFunction<ORDERNUM> FunctionType; \
  FunctionType::Pointer function = FunctionType::New(); \
  \
  function->Print( std::cout ); \
  for( unsigned j = 0; j < npoints; j++ ) \
    { \
    double results = function->Evaluate( x[j] ); \
    /* compare with external results */\
    if( vnl_math_abs( results - b##ORDERNUM[j] ) > 1e-6 ) \
      { \
      std::cout << "Error with " << ORDERNUM << " order BSplineKernelFunction" << std::endl; \
      std::cout << "Expected: " << b##ORDERNUM[j] << " but got " << results; \
      std::cout << " at x = " << x[j] << std::endl; \
      std::cout << "Test failed" << std::endl; \
      return EXIT_FAILURE; \
      } \
    } \
  }

  TEST_BSPLINE_KERNEL(0);
  TEST_BSPLINE_KERNEL(1);
  TEST_BSPLINE_KERNEL(2);
  TEST_BSPLINE_KERNEL(3);

  // Testing derivative
 {
  const unsigned int SplineOrder = 3;
  typedef itk::BSplineDerivativeKernelFunction<SplineOrder> DerivativeFunctionType;
  DerivativeFunctionType::Pointer derivFunction = DerivativeFunctionType::New();
  derivFunction->Print( std::cout );

  typedef itk::BSplineKernelFunction<SplineOrder - 1> FunctionType;
  FunctionType::Pointer function = FunctionType::New();

  double x = -0.25;
  double expectedValue = function->Evaluate( x + 0.5 ) -
    function->Evaluate( x - 0.5 );
  double results = derivFunction->Evaluate( x );

  if ( vnl_math_abs( results - expectedValue ) > 1e-6 )
    {
    std::cout << "Error with " << SplineOrder << " order BSplineDerivativeKernelFunction"
      << std::endl;
    std::cout << "Expected: " << expectedValue << " but got " << results;
    std::cout << " at x = " << x << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

 }


  // Testing case of unimplemented spline order
  {

  typedef itk::BSplineKernelFunction<7> FunctionType;
  FunctionType::Pointer function = FunctionType::New();
 
  bool pass = false;
  try
    {
    function->Evaluate( 0.0 );
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught expected exception" << std::endl;
    std::cout << err << std::endl;
    pass = true;
    }
  
  if( !pass )
    {
    std::cout << "Did not catch expected exception" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  }

  // Testing case of unimplemented spline order
  {

  typedef itk::BSplineDerivativeKernelFunction<5> FunctionType;
  FunctionType::Pointer function = FunctionType::New();
 
  bool pass = false;
  try
    {
    function->Evaluate( 0.0 );
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught expected exception" << std::endl;
    std::cout << err << std::endl;
    pass = true;
    }
  
  if( !pass )
    {
    std::cout << "Did not catch expected exception" << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  }


  std::cout << "Test passed. " << std::endl;
 return EXIT_SUCCESS;
}

