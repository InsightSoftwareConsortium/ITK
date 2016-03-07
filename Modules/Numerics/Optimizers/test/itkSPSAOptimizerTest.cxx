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
#include "itkSPSAOptimizer.h"


/**
 * \class
 *  The objective function is the quadratic form:
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
 */
class SPSACostFunction : public itk::SingleValuedCostFunction
{
 public:

  typedef SPSACostFunction               Self;
  typedef itk::SingleValuedCostFunction  Superclass;
  typedef itk::SmartPointer<Self>        Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  itkNewMacro( Self );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::MeasureType         MeasureType;


  SPSACostFunction()
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

int itkSPSAOptimizerTest(int, char* [] )
{
  std::cout << "SPSAOptimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::SPSAOptimizer                   OptimizerType;
  typedef  OptimizerType::ScalesType            ScalesType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // Declaration of the CostFunction
  SPSACostFunction::Pointer costFunction = SPSACostFunction::New();
  itkOptimizer->SetCostFunction( costFunction.GetPointer() );

  typedef SPSACostFunction::ParametersType    ParametersType;
  const unsigned int spaceDimension =
    costFunction->GetNumberOfParameters();

  ScalesType    parametersScale( spaceDimension );
  parametersScale[0] = 1.0;
  parametersScale[1] = 2.0;
  itkOptimizer->SetScales( parametersScale );

  itkOptimizer->MinimizeOn();
  itkOptimizer->SetMaximumNumberOfIterations(100);
  itkOptimizer->Seta( 1.0 );
  itkOptimizer->SetA( 10.0 );
  itkOptimizer->SetAlpha( 0.602 );
  itkOptimizer->Setc( 0.0001 );
  itkOptimizer->SetGamma( 0.101 );
  itkOptimizer->SetTolerance(1e-5);
  itkOptimizer->SetStateOfConvergenceDecayRate(0.5);
  itkOptimizer->SetMinimumNumberOfIterations(10);
  itkOptimizer->SetNumberOfPerturbations(1);

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );
  initialPosition[0] =  100;
  initialPosition[1] = -100;
  itkOptimizer->SetInitialPosition( initialPosition );

  try
    {
    itkOptimizer->GuessParameters(50, 70.0);
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Guessing Parameters" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "\nEstimated parameter: a = " << itkOptimizer->Geta();
  std::cout << "\nEstimated parameter: A = " << itkOptimizer->GetA() << "\n" << std::endl;

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

  std::cout
    << "StateOfConvergence in last iteration: "
    << itkOptimizer->GetStateOfConvergence()
    << std::endl;
  std::cout
    << "NumberOfIterations: "
    << itkOptimizer->GetCurrentIteration()
    << std::endl;

  std::cout
    << "Stop condition: "
    << itkOptimizer->GetStopConditionDescription()
    << std::endl;


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
  if (itkOptimizer->GetStopCondition() == itk::SPSAOptimizer::Unknown)
    {
    pass = false;
    }
  if (itkOptimizer->GetStopCondition() == itk::SPSAOptimizer::MetricError)
    {
    pass = false;
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  itkOptimizer->Print( std::cout );

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}
