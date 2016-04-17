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

#include "itkBSplineDerivativeKernelFunction.h"

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
    if( itk::Math::abs( results - b##ORDERNUM[j] ) > 1e-6 ) \
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

  // Testing derivative spline order = 0
 {
  const unsigned int SplineOrder = 0;
  typedef itk::BSplineDerivativeKernelFunction<SplineOrder> DerivativeFunctionType;
  DerivativeFunctionType::Pointer derivFunction = DerivativeFunctionType::New();
  derivFunction->Print( std::cout );

  double xx = -0.25;
  double expectedValue = 0.0;
  double results = derivFunction->Evaluate( xx );

  if ( itk::Math::abs( results - expectedValue ) > 1e-6 )
    {
    std::cout << "Error with " << SplineOrder << " order BSplineDerivativeKernelFunction"
      << std::endl;
    std::cout << "Expected: " << expectedValue << " but got " << results;
    std::cout << " at x = " << xx << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

 }

  // Testing derivative spline order = 1
 {
  const unsigned int SplineOrder = 1;
  typedef itk::BSplineDerivativeKernelFunction<SplineOrder> DerivativeFunctionType;
  DerivativeFunctionType::Pointer derivFunction = DerivativeFunctionType::New();

  typedef itk::BSplineKernelFunction<SplineOrder - 1> FunctionType;
  FunctionType::Pointer function = FunctionType::New();

  for( double xx = -3.0; xx <= 3.0; xx += 0.1 )
    {
    double expectedValue = function->Evaluate( xx + 0.5 ) -
      function->Evaluate( xx - 0.5 );
    double results = derivFunction->Evaluate( xx );

    if ( itk::Math::abs( results - expectedValue ) > 1e-6 )
      {
      std::cout << "Error with " << SplineOrder << " order BSplineDerivativeKernelFunction"
        << std::endl;
      std::cout << "Expected: " << expectedValue << " but got " << results;
      std::cout << " at x = " << xx << std::endl;
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
 }

  // Testing derivative spline order = 2
 {
  const unsigned int SplineOrder = 2;
  typedef itk::BSplineDerivativeKernelFunction<SplineOrder> DerivativeFunctionType;
  DerivativeFunctionType::Pointer derivFunction = DerivativeFunctionType::New();
  derivFunction->Print( std::cout );

  typedef itk::BSplineKernelFunction<SplineOrder - 1> FunctionType;
  FunctionType::Pointer function = FunctionType::New();

  for( double xx = -3.0; xx <= 3.0; xx += 0.1 )
    {
    double expectedValue = function->Evaluate( xx + 0.5 ) -
      function->Evaluate( xx - 0.5 );
    double results = derivFunction->Evaluate( xx );

    if ( itk::Math::abs( results - expectedValue ) > 1e-6 )
      {
      std::cout << "Error with " << SplineOrder << " order BSplineDerivativeKernelFunction"
        << std::endl;
      std::cout << "Expected: " << expectedValue << " but got " << results;
      std::cout << " at x = " << xx << std::endl;
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
 }

  // Testing derivative spline order = 3
 {
  const unsigned int SplineOrder = 3;
  typedef itk::BSplineDerivativeKernelFunction<SplineOrder> DerivativeFunctionType;
  DerivativeFunctionType::Pointer derivFunction = DerivativeFunctionType::New();
  derivFunction->Print( std::cout );

  typedef itk::BSplineKernelFunction<SplineOrder - 1> FunctionType;
  FunctionType::Pointer function = FunctionType::New();

  for( double xx = -3.0; xx <= 3.0; xx += 0.1 )
    {
    double expectedValue = function->Evaluate( xx + 0.5 ) -
      function->Evaluate( xx - 0.5 );
    double results = derivFunction->Evaluate( xx );

    if ( itk::Math::abs( results - expectedValue ) > 1e-6 )
      {
      std::cout << "Error with " << SplineOrder << " order BSplineDerivativeKernelFunction"
        << std::endl;
      std::cout << "Expected: " << expectedValue << " but got " << results;
      std::cout << " at x = " << xx << std::endl;
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
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
