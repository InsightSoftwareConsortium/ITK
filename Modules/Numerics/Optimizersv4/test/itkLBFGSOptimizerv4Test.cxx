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

#include "itkLBFGSOptimizerv4.h"
#include "itkMath.h"
#include "vnl/algo/vnl_lbfgs.h"
#include "itkTestingMacros.h"
#include "itkMath.h"
#include <iostream>

/**
 * \class itkLBFGSOptimizerv4TestMetric
 *
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
class itkLBFGSOptimizerv4TestMetric : public itk::ObjectToObjectMetricBase
{
public:

  typedef itkLBFGSOptimizerv4TestMetric     Self;
  typedef itk::ObjectToObjectMetricBase     Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );

  itkTypeMacro( itkLBFGSOptimizerv4TestMetric, ObjectToObjectMetricBase );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;
  typedef Superclass::MeasureType                 MeasureType;

  itkLBFGSOptimizerv4TestMetric()
  {
  m_HasLocalSupport = false;
  }

  virtual MeasureType GetValue() const ITK_OVERRIDE
  {
    double x = this->m_Parameters[0];
    double y = this->m_Parameters[1];

    std::cout << "GetValue ( " << x << " , " << y << ") = ";

    double val = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << val << std::endl;

    return val;
  }

  virtual void GetDerivative( DerivativeType  & derivative ) const ITK_OVERRIDE
  {
    double x = this->m_Parameters[0];
    double y = this->m_Parameters[1];

    std::cout << "GetDerivative ( " << x << " , " << y << ") = ";

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = -(3*x + 2*y -2);
    derivative[1] = -(2*x + 6*y +8);

    std::cout << "(" << derivative[0] <<" , " << derivative[1] << ")" << std::endl;
  }

  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE
  {
    value = GetValue();
    GetDerivative( derivative );
  }

  virtual void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE
  {
    m_Parameters.SetSize( SpaceDimension );
  }

  virtual Superclass::NumberOfParametersType GetNumberOfLocalParameters() const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  virtual Superclass::NumberOfParametersType GetNumberOfParameters(void) const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  virtual void SetParameters( ParametersType & params ) ITK_OVERRIDE
  {
    this->m_Parameters =  params;
  }

  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
  {
    return this->m_Parameters;
  }

  virtual bool HasLocalSupport() const ITK_OVERRIDE
  {
    return m_HasLocalSupport;
  }

  void SetHasLocalSupport(bool hls)
  {
    m_HasLocalSupport = hls;
  }

  virtual void UpdateTransformParameters( const DerivativeType &, ParametersValueType ) ITK_OVERRIDE
  {
  }

private:

  ParametersType  m_Parameters;
  bool            m_HasLocalSupport;

};


int itkLBFGSOptimizerv4Test(int, char* [] )
{
  std::cout << "LBFGS Optimizerv4 Test \n \n";

  typedef  itk::LBFGSOptimizerv4                OptimizerType;
  typedef  vnl_lbfgs                            vnlOptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // Declaration of the metric
  itkLBFGSOptimizerv4TestMetric::Pointer metric = itkLBFGSOptimizerv4TestMetric::New();

  // Set some optimizer parameters
  itkOptimizer->SetTrace( false );
  itkOptimizer->SetMaximumNumberOfFunctionEvaluations( 1000 );
  itkOptimizer->SetGradientConvergenceTolerance( 1e-3 );
  itkOptimizer->SetLineSearchAccuracy( 0.1 );
  itkOptimizer->SetDefaultStepLength( 5.0 );
  std::cout << "GetValue() before optimizer starts: " << itkOptimizer->GetValue() << std::endl;
  std::cout << "SetMetric." << std::endl;
  itkOptimizer->SetMetric( metric.GetPointer() );

  const double G_Tolerance      = 1e-4;  // Gradient magnitude tolerance
  const int    Max_Iterations   = 100;   // Maximum number of iterations
  const bool   Trace            = false; // Tracing
  const double LineSearch_Tol   = 0.9;   // Line search tolerance
  const double Step_Length      = 1.0;   // Default step length

  std::cout << "Get vnl optimizer." << std::endl;
  vnlOptimizerType * vnlOptimizer = itkOptimizer->GetOptimizer();

  vnlOptimizer->set_check_derivatives( 0 );

  const unsigned int SpaceDimension = 2;
  OptimizerType::ParametersType initialValue(SpaceDimension);

  // We start not so far from  | 2 -2 |
  initialValue[0] =  100;
  initialValue[1] = -100;

  // Set the initial position by setting the metric
  // parameters.
  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters( initialValue );

  // Set some optimizer parameters
  itkOptimizer->SetTrace( Trace );
  itkOptimizer->SetMaximumNumberOfFunctionEvaluations( Max_Iterations );
  itkOptimizer->SetGradientConvergenceTolerance( G_Tolerance );
  itkOptimizer->SetLineSearchAccuracy( LineSearch_Tol );
  itkOptimizer->SetDefaultStepLength( Step_Length );
  itkOptimizer->Print( std::cout );
  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

  std::cout << "Start optimization." << std::endl;
  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation()    << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "End condition   = " << vnlOptimizer->get_failure_code()    << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer->get_num_iterations()  << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer->get_num_evaluations() << std::endl;
  std::cout << std::endl;

  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;

  std::cout << "End condition   = " << itkOptimizer->GetStopConditionDescription() << std::endl;
  std::cout << "Trace   = " << itkOptimizer->GetTrace() << std::endl;
  std::cout << "LineSearchAccuracy   = " << itkOptimizer->GetLineSearchAccuracy() << std::endl;
  std::cout << "GradientConvergenceTolerance   = " << itkOptimizer->GetGradientConvergenceTolerance() << std::endl;
  std::cout << "DefaultStepLength   = " << itkOptimizer->GetDefaultStepLength() << std::endl;
  std::cout << "MaximumNumberOfFunctionEvaluations   = " << itkOptimizer->GetMaximumNumberOfFunctionEvaluations() << std::endl;
  std::cout << "NumberOfIterations  = " << itkOptimizer->GetCurrentIteration() << std::endl;

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[2] = { 2, -2 };
  for( unsigned int j = 0; j < 2; ++j )
    {
    if( itk::Math::FloatAlmostEqual( finalPosition[j], trueParameters[j] ) )
      {
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Get the final value of the optimizer
  std::cout << "Testing GetValue() : ";
  OptimizerType::MeasureType finalValue = itkOptimizer->GetValue();
  if( std::fabs( finalValue + 10.0 ) > 0.01)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  //
  // Test stopping when number of iterations reached
  //
  itkOptimizer->SetNumberOfIterations( 5 );
  metric->SetParameters( initialValue );

   try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

   std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;
   std::cout << "NumberOfIterations  = " << itkOptimizer->GetCurrentIteration() << std::endl;

  if ( itkOptimizer->GetCurrentIteration() != 5 )
     {
     std::cout << "Not expected number of iterations!" << std::endl;
     std::cout << "[FAILURE]" << std::endl;
     return EXIT_FAILURE;
     }


  // Test with non-indentity scales
  //
  std::cout << std::endl << "Test with non-identiy scales." << std::endl;
  OptimizerType::ScalesType scales(2);
  scales[0] = 1.5;
  scales[1] = 0.75;
  itkOptimizer->SetScales( scales );

  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters( initialValue );

  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation()    << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Scales after optimization: " << itkOptimizer->GetScales() << std::endl;
  finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;

  // check results to see if it is within range
  pass = true;
  for( unsigned int j = 0; j < 2; ++j )
    {
    if( itk::Math::FloatAlmostEqual( finalPosition[j], trueParameters[j] ) )
      {
      pass = false;
      }
    }

  if( !pass )
    {
    std::cerr << "Test failed. finalPosition is not correct." << std::endl;
    return EXIT_FAILURE;
    }

  // Test with local-support transform. Should FAIL.
  // Such transforms are not yet supported.
  metric->SetHasLocalSupport( true );
  TRY_EXPECT_EXCEPTION( itkOptimizer->StartOptimization() );

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
