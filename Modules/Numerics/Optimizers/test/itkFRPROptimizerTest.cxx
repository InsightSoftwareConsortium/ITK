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
#include "itkFRPROptimizer.h"


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
 * \class FRPRGradientCostFunction
 */
class FRPRGradientCostFunction : public itk::SingleValuedCostFunction
{
public:

  typedef FRPRGradientCostFunction      Self;
  typedef itk::SingleValuedCostFunction Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( FRPRGradientCostFunction, SingleValuedCostFunction );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::MeasureType         MeasureType;

  FRPRGradientCostFunction()
  {
  }


  MeasureType  GetValue( const ParametersType & parameters ) const ITK_OVERRIDE
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
                            DerivativeType & derivative ) const ITK_OVERRIDE
  {

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetDerivative( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    DerivativeType temp(SpaceDimension);
    temp.Fill( 0 );
    derivative = temp;
    derivative[0] = 3 * x + 2 * y -2;
    derivative[1] = 2 * x + 6 * y +8;

    std::cout << derivative << std::endl;

  }


  unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
    {
    return SpaceDimension;
    }

private:


};

int itkFRPROptimizerTest(int, char* [] )
{
  std::cout << "FRPR Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::FRPROptimizer  OptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction
  FRPRGradientCostFunction::Pointer costFunction = FRPRGradientCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction.GetPointer() );


  typedef FRPRGradientCostFunction::ParametersType    ParametersType;

  const unsigned int spaceDimension =
                      costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;

  itkOptimizer->SetStepLength(0.01);
  itkOptimizer->SetMaximize(false);
  itkOptimizer->SetMaximumIteration( 50 );

  {
  // Exercise the methods that set the optimization mode
  std::cout << "Testing Fletch Reeves Mode" << std::endl;
  itkOptimizer->SetToFletchReeves();

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
      pass = false;
    }

  // Exercise various member functions.
  std::cout << "Maximize: " << itkOptimizer->GetMaximize() << std::endl;
  std::cout << std::endl;
  std::cout << "MaximumIteration: " << itkOptimizer->GetMaximumIteration();
  std::cout << std::endl;

  itkOptimizer->Print( std::cout );

  if( !pass )
    {
    std::cout << "Fletch Reeves test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Fletch Reeves test passed." << std::endl;
  }

  {
  // Exercise the methods that set the optimization mode
  std::cout << "Testing Polak Ribiere Mode" << std::endl;
  itkOptimizer->SetToPolakRibiere();

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
      pass = false;
    }

  // Exercise various member functions.
  std::cout << "Maximize: " << itkOptimizer->GetMaximize() << std::endl;
  std::cout << std::endl;
  std::cout << "MaximumIteration: " << itkOptimizer->GetMaximumIteration();
  std::cout << std::endl;

  itkOptimizer->Print( std::cout );

  if( !pass )
    {
    std::cout << "Polak Ribiere test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Polak Ribiere test passed." << std::endl;
  }

  return EXIT_SUCCESS;
}
