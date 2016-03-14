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

#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkNormalVariateGenerator.h"
#include "itkCommand.h"
#include "itkMath.h"

namespace itk
{

/**
 * \class OnePlusOneCostFunction
 *
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
 */
class OnePlusOneCostFunction : public itk::SingleValuedCostFunction
{
public:

  typedef OnePlusOneCostFunction          Self;
  typedef itk::SingleValuedCostFunction   Superclass;
  typedef itk::SmartPointer<Self>         Pointer;
  typedef itk::SmartPointer<const Self>   ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( OnePlusOneCostFunction, SingleValuedCostFunction );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::MeasureType         MeasureType;

  OnePlusOneCostFunction()
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

  void GetDerivative(const ParametersType & itkNotUsed( parameters ),
                           DerivativeType & itkNotUsed( derivative ) ) const ITK_OVERRIDE
  {
    itkGenericExceptionMacro("OnePlusOneEvolutionaryOptimizer is not supposed to call GetDerivative()");
  }

  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
    {
    return SpaceDimension;
    }

private:
};

class OnePlusOneCommandIterationUpdate : public itk::Command
{
public:
  typedef  OnePlusOneCommandIterationUpdate   Self;
  typedef  itk::Command                       Superclass;
  typedef itk::SmartPointer<Self>             Pointer;
  itkNewMacro( Self );

protected:
  OnePlusOneCommandIterationUpdate() { m_LastMetricValue = 0.0; };

public:
  typedef itk::OnePlusOneEvolutionaryOptimizer     OptimizerType;
  typedef   const OptimizerType *                  OptimizerPointer;

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
      OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
      if( ! itk::IterationEvent().CheckEvent( &event ) )
        {
        return;
        }
      double currentValue = optimizer->GetValue();
      // Only print out when the Metric value changes
      if( std::fabs( m_LastMetricValue - currentValue ) > 1e-7 )
        {
        std::cout << optimizer->GetCurrentIteration() << "   ";
        std::cout << currentValue << "   ";
        std::cout << optimizer->GetCurrentPosition() << std::endl;
        m_LastMetricValue = currentValue;
        }
    }

private:
  double m_LastMetricValue;
};

}


int itkOnePlusOneEvolutionaryOptimizerTest(int, char* [] )
{
  std::cout << "Gradient Descent Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::OnePlusOneEvolutionaryOptimizer  OptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  itk::OnePlusOneCommandIterationUpdate::Pointer observer = itk::OnePlusOneCommandIterationUpdate::New();
  itkOptimizer->AddObserver( itk::IterationEvent(), observer );

  // Declaration of the CostFunction
  itk::OnePlusOneCostFunction::Pointer costFunction = itk::OnePlusOneCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction.GetPointer() );


  typedef itk::OnePlusOneCostFunction::ParametersType    ParametersType;

  const unsigned int spaceDimension = costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;

  itkOptimizer->MinimizeOn();
  itkOptimizer->Initialize( 10 );
  itkOptimizer->SetEpsilon( 0.1 );
  itkOptimizer->SetMaximumIteration( 8000 );


  typedef itk::Statistics::NormalVariateGenerator  GeneratorType;
  GeneratorType::Pointer generator = GeneratorType::New();
  itkOptimizer->SetNormalVariateGenerator( generator );

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

  // Exercise various member functions.
  std::cout << "Maximize: " << itkOptimizer->GetMaximize() << std::endl;
  std::cout << "Epsilon: " << itkOptimizer->GetEpsilon() << std::endl;
  std::cout << "NumberOfIterations: " << itkOptimizer->GetMaximumIteration() << std::endl;

  itkOptimizer->Print( std::cout );
  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
