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
#include "itkGaussianDerivativeOperator.h"
#include "itkStdStreamStateSave.h"

namespace
{

bool TestGaussianOperator( double variance,
                           double error,
                           unsigned int width,
                           unsigned int order )
{

  typedef itk::GaussianDerivativeOperator< double, 1 > GaussianOp;

  std::cout << "Testing variance: " << variance
            << " error: " << error
            << " width: " << width
            << " order: " << order
            << std::endl;

  GaussianOp op;

  op.SetVariance( variance );
  op.SetMaximumError( error );
  op.SetMaximumKernelWidth( width );

  op.SetOrder( order );
  op.SetNormalizeAcrossScale( false );

  op.CreateDirectional();

  std::cout.precision(16);

  double total = std::accumulate( op.Begin(), op.End(), 0.0 );

  std::cout << "total: " << total << std::endl;

  if ( order == 0 && std::abs(total - 1.0) > itk::NumericTraits<double>::epsilon()*32 )
    {
    std::cerr << "FAILURE: expected coefficients to sum to 1.0! Actual: " << total << std::endl;
    }
  else if ( order != 0 && std::abs(total) > itk::NumericTraits<double>::epsilon()*32 )
    {
    std::cerr << "FAILURE: expected coefficients to sum to 0.0! Actual: " << total << std::endl;
    }
  else
    {
    return true;
    }

    std::cout << "---operator---" << std::endl;
    GaussianOp::Iterator i = op.Begin();
    i += op.Size()/2;
    for(; i != op.End(); ++i )
      {
      std::cout << *i << std::endl;
      }
    std::cout << "---end--" << std::endl;

    return false;

}

}

int itkGaussianDerivativeOperatorTest( int argc, char *argv[] )
{

// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  if (argc == 5 )
    {
    double variance = atof(argv[1]);
    double error = atof(argv[2]);
    unsigned int width = atoi(argv[3]);
    unsigned int order = atoi(argv[4]);

    TestGaussianOperator( variance, error, width, order );
    return EXIT_FAILURE;
    }
  else if ( argc > 1 )
    {
    std::cerr << "Usage: " << argv[0] << " [ variance error width order ]" << std::endl;
    return EXIT_FAILURE;
    }

  // At this point, obviously, argc <= 1. In some scenario's, argc == 0, typically when
  // the test function is called from the interactive TestDriver commandline interface,
  // by having the user entering its test number. On the other hand, argc == 1 when the
  // the TestDriver has the name of the test function as its only commandline argument.
  // In either way the tests below here should be performed.

  // Exercise code

  typedef itk::GaussianDerivativeOperator< double, 3 > GaussianOp;

  GaussianOp op1;
  GaussianOp op2;

  // print self method
  std::cout << op1;

  // assignement
  op2 = op1;


  bool pass = true;

  std::cout << "====== DerivativeOperator ======" << std::endl;

  pass &=   TestGaussianOperator( .2, .001, 30, 0 );
  pass &=   TestGaussianOperator( .2, .001, 30, 1 );
  pass &=   TestGaussianOperator( .2, .001, 30, 2 );
  pass &=   TestGaussianOperator( .2, .001, 30, 3 );
  pass &=   TestGaussianOperator( .2, .001, 30, 4 );

  pass &=   TestGaussianOperator( 1, .001, 30, 0 );
  pass &=   TestGaussianOperator( 1, .001, 30, 1 );
  pass &=   TestGaussianOperator( 1, .001, 30, 2 );
  pass &=   TestGaussianOperator( 1, .001, 30, 3 );
  pass &=   TestGaussianOperator( 1, .001, 30, 4 );

  pass &=   TestGaussianOperator( 10, .001, 30, 0 );
  pass &=   TestGaussianOperator( 10, .001, 30, 1 );

  pass &=   TestGaussianOperator( 10, .0001, 100, 1 );

  pass &=   TestGaussianOperator( 50, .001, 300, 0 );

  if ( pass )
    {
    return EXIT_SUCCESS;
    }
  return EXIT_FAILURE;

}
