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

#include "itkPowellOptimizer.h"

int POWELL_CALLS_TO_GET_VALUE = 0;

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
 * \class PowellBoundedCostFunction
 *
 */
class PowellBoundedCostFunction : public itk::SingleValuedCostFunction
{
public:

  typedef PowellBoundedCostFunction     Self;
  typedef itk::SingleValuedCostFunction Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( PowellBoundedCostFunction, SingleValuedCostFunction );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::MeasureType         MeasureType;

  PowellBoundedCostFunction()
  {
  }


  void GetDerivative( const ParametersType & ,
                      DerivativeType &  ) const ITK_OVERRIDE
  {
  }

  virtual MeasureType  GetValue( const ParametersType & parameters ) const ITK_OVERRIDE
  {
    ++POWELL_CALLS_TO_GET_VALUE;

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "      GetValue( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    MeasureType measure = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << measure << std::endl;

    return measure;

  }

  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
    {
    return SpaceDimension;
    }

private:


};

int itkPowellOptimizerTest(int, char* [] )
{
  std::cout << "Powell Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::PowellOptimizer  OptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction
  PowellBoundedCostFunction::Pointer costFunction = PowellBoundedCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction.GetPointer() );


  typedef PowellBoundedCostFunction::ParametersType    ParametersType;

  const unsigned int spaceDimension =
                      costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;

  itkOptimizer->SetMaximize(false);
  itkOptimizer->SetStepLength( 10 );
  itkOptimizer->SetStepTolerance( 0.01 );
  itkOptimizer->SetValueTolerance( 0.1 );
  itkOptimizer->SetMaximumIteration( 100 );

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
  std::cout << "StepLength: " << itkOptimizer->GetStepLength();
  std::cout << std::endl;
  std::cout << "CurrentIteration: " << itkOptimizer->GetCurrentIteration();
  std::cout << std::endl;

  itkOptimizer->Print( std::cout );

  std::cout << "Calls to GetValue = " << POWELL_CALLS_TO_GET_VALUE << std::endl;

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}
