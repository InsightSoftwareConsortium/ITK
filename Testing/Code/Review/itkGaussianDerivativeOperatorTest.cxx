#include "itkGaussianOperator.h"
#include "itkGaussianDerivativeOperator.h"

#include <iterator>
#include <numeric>


namespace
{

bool TestGaussianOperator( double variance,
                           double error,
                           unsigned int width,
                           unsigned int order,
                           bool useDerivativeOperator )
{

  typedef itk::GaussianDerivativeOperator< double, 1 > GaussianOp;

  std::cout << "Testing variance: " << variance
            << " error: " << error
            << " width: " << width
            << " order: " << order
            << " derivativeOperator: " << useDerivativeOperator
            << std::endl;

  GaussianOp op;

  op.SetVariance( variance );
  op.SetMaximumError( error );
  op.SetMaximumKernelWidth( width );

  op.SetOrder( order );
  op.SetNormalizeAcrossScale( false );
  op.SetUseDerivativeOperator( useDerivativeOperator );

  op.CreateDirectional();

  std::cout.precision(16);

  double total = std::accumulate( op.Begin(), op.End(), 0.0 );

  std::cout << "total: " << total << std::endl;

  if ( order == 0 && vcl_abs(total - 1.0) > itk::NumericTraits<double>::epsilon()*8 )
    {
    std::cerr << "FAILURE: expected coefficients to sum to 1.0! Actual: " << total << std::endl;
    }
  else if ( order != 0 && vcl_abs(total) > itk::NumericTraits<double>::epsilon()*8 )
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

  if (argc == 5 )
    {
    double variance = atof(argv[1]);
    double error = atof(argv[2]);
    unsigned int width = atoi(argv[3]);
    unsigned int order = atoi(argv[4]);

    TestGaussianOperator( variance, error, width, order, true );
    TestGaussianOperator( variance, error, width, order, false );

    return EXIT_SUCCESS;
    }
  else if ( argc != 1 )
    {
    std::cerr << "Usage: " << argv[0] << " [ variance error width order ]" << std::endl;
    return EXIT_FAILURE;
    }


  // Excercise code

  typedef itk::GaussianDerivativeOperator< double, 3 > GaussianOp;

  GaussianOp op1;
  GaussianOp op2;

  // print self method
  std::cout << op1;

  // assignement
  op2 = op1;


  bool pass = true;

  std::cout << "====== DerivativeOperator ======" << std::endl;

  pass &=   TestGaussianOperator( .2, .001, 30, 0, true );
  pass &=   TestGaussianOperator( .2, .001, 30, 1, true );
  pass &=   TestGaussianOperator( .2, .001, 30, 2, true );
  pass &=   TestGaussianOperator( .2, .001, 30, 3, true );
  pass &=   TestGaussianOperator( .2, .001, 30, 4, true );

  pass &=   TestGaussianOperator( 1, .001, 30, 0, true );
  pass &=   TestGaussianOperator( 1, .001, 30, 1, true );
  pass &=   TestGaussianOperator( 1, .001, 30, 2, true );
  pass &=   TestGaussianOperator( 1, .001, 30, 3, true );
  pass &=   TestGaussianOperator( 1, .001, 30, 4, true );

  pass &=   TestGaussianOperator( 10, .001, 30, 0, true );
  pass &=   TestGaussianOperator( 10, .001, 30, 1, true );

  pass &=   TestGaussianOperator( 10, .0001, 100, 1, true );

  pass &=   TestGaussianOperator( 50, .001, 300, 0, true );

  std::cout << "====== Analytic Derivative ======" << std::endl;

  pass &=   TestGaussianOperator( .2, .001, 30, 0, false );
  pass &=   TestGaussianOperator( .2, .001, 30, 1, false );
  pass &=   TestGaussianOperator( .2, .001, 30, 2, false );
  pass &=   TestGaussianOperator( .2, .001, 30, 3, false );
  pass &=   TestGaussianOperator( .2, .001, 30, 4, false );

  pass &=   TestGaussianOperator( 1, .001, 30, 0, false );
  pass &=   TestGaussianOperator( 1, .001, 30, 1, false );
  pass &=   TestGaussianOperator( 1, .001, 30, 2, false );
  pass &=   TestGaussianOperator( 1, .001, 30, 3, false );
  pass &=   TestGaussianOperator( 1, .001, 30, 4, false );


  pass &=   TestGaussianOperator( 10, .001, 30, 0, false );
  pass &=   TestGaussianOperator( 10, .001, 30, 1, false );

  pass &=   TestGaussianOperator( 10, .0001, 100, 1, false );

  pass &=   TestGaussianOperator( 50, .001, 300, 0, true );


  if ( pass )
    return EXIT_SUCCESS;
  return EXIT_FAILURE;

}
