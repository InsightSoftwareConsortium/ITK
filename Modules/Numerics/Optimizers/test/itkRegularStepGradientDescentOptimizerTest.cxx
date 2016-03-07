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

#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkMath.h"

/**
 *  The objectif function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is a matrix and b is a vector
 *  The system in this example is:
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *
 *   the solution is the vector | 2 -2 |
 *
 * \class RSGCostFunction
 */
class RSGCostFunction : public itk::SingleValuedCostFunction
{
public:

  typedef RSGCostFunction               Self;
  typedef itk::SingleValuedCostFunction Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::MeasureType         MeasureType;


  RSGCostFunction()
  {
  }


  virtual MeasureType  GetValue( const ParametersType & parameters ) const ITK_OVERRIDE
  {

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetValue( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    MeasureType measure = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << measure << std::endl;
    return measure;

  }

  void GetDerivative( const ParametersType & parameters,
                            DerivativeType  & derivative ) const ITK_OVERRIDE
  {

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetDerivative( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    derivative = DerivativeType( SpaceDimension );
    derivative[0] = 3 * x + 2 * y -2;
    derivative[1] = 2 * x + 6 * y +8;

  }


  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
    {
    return SpaceDimension;
    }

private:
};

int itkRegularStepGradientDescentOptimizerTest(int, char* [] )
{
  std::cout << "RegularStepGradientDescentOptimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::RegularStepGradientDescentOptimizer  OptimizerType;

  typedef  OptimizerType::ScalesType            ScalesType;


  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction
  RSGCostFunction::Pointer costFunction = RSGCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction.GetPointer() );


  typedef RSGCostFunction::ParametersType    ParametersType;


  const unsigned int spaceDimension =
                      costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );
  initialPosition[0] =  100;
  initialPosition[1] = -100;

  ScalesType    parametersScale( spaceDimension );
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  itkOptimizer->MinimizeOn();
  itkOptimizer->SetScales( parametersScale );
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-6 );
  itkOptimizer->SetMaximumStepLength( 30.0 );
  itkOptimizer->SetMinimumStepLength( 1e-6 );
  itkOptimizer->SetNumberOfIterations( 900 );

  itkOptimizer->SetInitialPosition( initialPosition );

  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }


  ParametersType finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[2] = { 2, -2 };
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( itk::Math::abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      {
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  // Run now with a different relaxation factor

  {
  itkOptimizer->SetInitialPosition( initialPosition );

  itkOptimizer->SetRelaxationFactor( 0.8 );
  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;

  //
  // check results to see if it is within range
  //
  pass = true;
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( itk::Math::abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      {
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  }

  //
  // Verify that the optimizer doesn't run if the
  // number of iterations is set to zero.
  //
  {
  itkOptimizer->SetNumberOfIterations( 0 );
  itkOptimizer->SetInitialPosition( initialPosition );

  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    return EXIT_FAILURE;
    }

  if( itkOptimizer->GetCurrentIteration() > 0 )
    {
    std::cerr << "The optimizer is running iterations despite of ";
    std::cerr << "having a maximum number of iterations set to zero" << std::endl;
    return EXIT_FAILURE;
    }
  }

  //
  // Test the Exception if the GradientMagnitudeTolerance is set to a negative value
  //
  itkOptimizer->SetGradientMagnitudeTolerance( -1.0 );
  bool expectedExceptionReceived = false;
  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & excp )
    {
    expectedExceptionReceived = true;
    std::cout << "Expected Exception " << std::endl;
    std::cout << excp << std::endl;
    }

  if( !expectedExceptionReceived )
    {
    std::cerr << "Failure to produce an exception when";
    std::cerr << "the GradientMagnitudeTolerance is negative " << std::endl;
    std::cerr << "TEST FAILED !" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "TEST PASSED !" << std::endl;
  return EXIT_SUCCESS;

}
