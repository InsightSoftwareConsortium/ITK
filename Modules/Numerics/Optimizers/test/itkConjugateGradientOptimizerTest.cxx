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

#include "itkConjugateGradientOptimizer.h"
#include "itkMath.h"


/**
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
 * \class conjugateCostFunction
 */
class conjugateCostFunction : public itk::SingleValuedCostFunction
{
public:

  typedef conjugateCostFunction             Self;
  typedef itk::SingleValuedCostFunction     Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( conjugateCostFunction, SingleValuedCostFunction );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;

  typedef vnl_vector<double>                      VectorType;
  typedef vnl_matrix<double>                      MatrixType;

  typedef double MeasureType;


  conjugateCostFunction()
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
                            DerivativeType & derivative ) const ITK_OVERRIDE
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

class CommandIterationUpdateConjugateGradient : public itk::Command
{
public:
  typedef  CommandIterationUpdateConjugateGradient   Self;
  typedef  itk::Command                              Superclass;
  typedef itk::SmartPointer<Self>                    Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdateConjugateGradient()
  {
    m_IterationNumber=0;
  }

public:
  typedef itk::ConjugateGradientOptimizer   OptimizerType;
  typedef   const OptimizerType   *         OptimizerPointer;

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
      Execute( (const itk::Object *)caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
      OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
      if( m_FunctionEvent.CheckEvent( &event ) )
        {
        std::cout << m_IterationNumber++ << "   ";
        std::cout << optimizer->GetCachedValue() << "   ";
        std::cout << optimizer->GetCachedCurrentPosition() << std::endl;
        }
      else if( m_GradientEvent.CheckEvent( &event ) )
        {
        std::cout << "Gradient " << optimizer->GetCachedDerivative() << "   ";
        }

    }

private:
  unsigned long m_IterationNumber;

  itk::FunctionEvaluationIterationEvent m_FunctionEvent;
  itk::GradientEvaluationIterationEvent m_GradientEvent;
};

int itkConjugateGradientOptimizerTest(int, char* [] )
{
  std::cout << "Conjugate Gradient Optimizer Test \n \n";

  typedef  itk::ConjugateGradientOptimizer  OptimizerType;

  typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  conjugateCostFunction::Pointer costFunction = conjugateCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction.GetPointer() );


  vnlOptimizerType * vnlOptimizer = itkOptimizer->GetOptimizer();

  const double F_Tolerance      = 1e-3;  // Function value tolerance
  const double G_Tolerance      = 1e-4;  // Gradient magnitude tolerance
  const double X_Tolerance      = 1e-8;  // Search space tolerance
  const double Epsilon_Function = 1e-10; // Step
  const int    Max_Iterations   =   100; // Maximum number of iterations

  vnlOptimizer->set_f_tolerance( F_Tolerance );
  vnlOptimizer->set_g_tolerance( G_Tolerance );
  vnlOptimizer->set_x_tolerance( X_Tolerance );
  vnlOptimizer->set_epsilon_function( Epsilon_Function );
  vnlOptimizer->set_max_function_evals( Max_Iterations );

  vnlOptimizer->set_check_derivatives( 3 );


  OptimizerType::ParametersType initialValue(2);       // constructor requires vector size
  // We start not so far from  | 2 -2 |
  initialValue[0] =  100;
  initialValue[1] = -100;


  OptimizerType::ParametersType currentValue(2);

  currentValue = initialValue;

  itkOptimizer->SetInitialPosition( currentValue );

  CommandIterationUpdateConjugateGradient::Pointer observer =
    CommandIterationUpdateConjugateGradient::New();
  itkOptimizer->AddObserver( itk::IterationEvent(), observer );
  itkOptimizer->AddObserver( itk::FunctionEvaluationIterationEvent(), observer );


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


  std::cout << "Number of iters = " << itkOptimizer->GetCurrentIteration()  << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer->get_num_evaluations() << std::endl;

  std::cout << "Report from vnl optimizer: " << std::endl;
  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

  std::cout << std::endl;

  //
  // check results to see if it is within range
  //

  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;

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
