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

#include "itkLBFGSOptimizer.h"
#include "itkMath.h"
#include <iostream>

/**
 * \class A cost function
 *  The objectif function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is represented as an itkMatrix and
 *  b is represented as a itkVector
 *
 *  The system in this example is:
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *
 *   the solution is the vector | 2 -2 |
 *
 */
class LBFGSCostFunction : public itk::SingleValuedCostFunction
{
public:

  typedef LBFGSCostFunction                 Self;
  typedef itk::SingleValuedCostFunction     Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( LBFCostFunction, SingleValuedCostFunction );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;

  typedef vnl_vector<double>                      VectorType;
  typedef vnl_matrix<double>                      MatrixType;

  typedef double MeasureType;

  LBFGSCostFunction()
  {
  }

  virtual double GetValue( const ParametersType & position ) const ITK_OVERRIDE
  {
    double x = position[0];
    double y = position[1];

    std::cout << "GetValue ( ";
    std::cout << x << " , " << y;
    std::cout << ") = ";

    double val = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << val << std::endl;

    return val;
  }

  void GetDerivative( const ParametersType & position,
                            DerivativeType  & derivative ) const ITK_OVERRIDE
  {
    double x = position[0];
    double y = position[1];

    std::cout << "GetDerivative ( ";
    std::cout << x << " , " << y;
    std::cout << ") = ";

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = 3*x + 2*y -2;
    derivative[1] = 2*x + 6*y +8;
    std::cout << "(";
    std::cout << derivative[0] <<" , ";
    std::cout << derivative[1] << ")" << std::endl;
  }


  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
    {
    return SpaceDimension;
    }

private:


};


int itkLBFGSOptimizerTest(int, char* [] )
{
  std::cout << "LBFGS Optimizer Test \n \n";

  typedef  itk::LBFGSOptimizer                   OptimizerType;
  typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // Declaration of the CostFunction adapter
  LBFGSCostFunction::Pointer costFunction = LBFGSCostFunction::New();

  // Set some optimizer parameters
  itkOptimizer->SetTrace( false );
  itkOptimizer->SetMaximumNumberOfFunctionEvaluations( 1000 );
  itkOptimizer->SetGradientConvergenceTolerance( 1e-3 );
  itkOptimizer->SetLineSearchAccuracy( 0.1 );
  itkOptimizer->SetDefaultStepLength( 5.0 );
  std::cout << "GetValue() before optimizer starts: " << itkOptimizer->GetValue() << std::endl;
  itkOptimizer->SetCostFunction( costFunction.GetPointer() );

  const double G_Tolerance      = 1e-4;  // Gradient magnitude tolerance
  const int    Max_Iterations   = 100;   // Maximum number of iterations
  const bool   Trace            = false; // Tracing
  const double LineSearch_Tol   = 0.9;   // Line search tolerance
  const double Step_Length      = 1.0;   // Default step length

  // const double F_Tolerance      = 1e-3;  // Function value tolerance: not used
  // const double X_Tolerance      = 1e-8;  // Search space tolerance: not used
  // const double Epsilon_Function = 1e-10; // Step : not used

  vnlOptimizerType * vnlOptimizer = itkOptimizer->GetOptimizer();

  vnlOptimizer->set_check_derivatives( 0 );

  const unsigned int SpaceDimension = 2;
  OptimizerType::ParametersType initialValue(SpaceDimension);

  // We start not so far from  | 2 -2 |
  initialValue[0] =  100;
  initialValue[1] = -100;

  OptimizerType::ParametersType currentValue(2);

  currentValue = initialValue;

  itkOptimizer->SetInitialPosition( currentValue );

  // Set some optimizer parameters
  itkOptimizer->SetTrace( Trace );
  itkOptimizer->SetMaximumNumberOfFunctionEvaluations( Max_Iterations );
  itkOptimizer->SetGradientConvergenceTolerance( G_Tolerance );
  itkOptimizer->SetLineSearchAccuracy( LineSearch_Tol );
  itkOptimizer->SetDefaultStepLength( Step_Length );
  itkOptimizer->Print( std::cout );
  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

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

  std::cout << "End condition   = " << vnlOptimizer->get_failure_code()    << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer->get_num_iterations()  << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer->get_num_evaluations() << std::endl;
  std::cout << std::endl;

  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  std::cout << "Solution        = ("
    << finalPosition[0] << ","
    << finalPosition[1] << ")" << std::endl;

  std::cout << "End condition   = "
    << itkOptimizer->GetStopConditionDescription() << std::endl;
  std::cout << "Trace   = " << itkOptimizer->GetTrace() << std::endl;
  std::cout << "LineSearchAccuracy   = "
    << itkOptimizer->GetLineSearchAccuracy() << std::endl;
  std::cout << "GradientConvergenceTolerance   = "
    << itkOptimizer->GetGradientConvergenceTolerance() << std::endl;
  std::cout << "DefaultStepLength   = "
    << itkOptimizer->GetDefaultStepLength() << std::endl;
  std::cout << "MaximumNumberOfFunctionEvaluations   = "
    << itkOptimizer->GetMaximumNumberOfFunctionEvaluations() << std::endl;

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

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Get the final value of the optimizer
  std::cout << "Testing GetValue() : ";
  OptimizerType::MeasureType finalValue = itkOptimizer->GetValue();
  if(std::fabs(finalValue+10.0)>0.01)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
