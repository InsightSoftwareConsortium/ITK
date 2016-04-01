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

#include "itkLBFGSBOptimizerv4.h"
#include "itkTextOutput.h"
#include "itkMath.h"
#include "itkTestingMacros.h"
#include "itkMath.h"
#include <iostream>

/**
 *  LBFGSBOptimizerv4TestMetric
 *
 *  The objective function is the quadratic form:
 *
 *  f(x) = 1/2 x^T A x - b^T x  subject to  -1 <= x <= 10
 *
 *  Where A is represented as an itkMatrix and
 *  b is represented as a itkVector
 *
 *  The system in this example is:
 *
 *          | 3  2 |       | 2|
 *      A=  | 2  6 |   b = |-8|
 *
 *   the solution is the vector | 4/3 -1 |
 *
 * \class LBFGSBOptimizerv4TestMetric
 */
class LBFGSBOptimizerv4TestMetric : public itk::ObjectToObjectMetricBase
{
public:

  typedef LBFGSBOptimizerv4TestMetric       Self;
  typedef itk::ObjectToObjectMetricBase     Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( LBFGSBOptimizerv4TestMetric, ObjectToObjectMetricBase );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;
  typedef Superclass::MeasureType                 MeasureType;

  typedef vnl_vector<double>                      VectorType;
  typedef vnl_matrix<double>                      MatrixType;

  LBFGSBOptimizerv4TestMetric():
  m_Parameters(0)
  {
    m_HasLocalSupport = false;
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

  virtual MeasureType GetValue() const ITK_OVERRIDE
  {

    double x = m_Parameters[0];
    double y = m_Parameters[1];

    double val = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << "GetValue ( " << x << " , " << y << ") = " << val << std::endl;

    return val;
  }

  virtual void GetDerivative( DerivativeType  & derivative ) const ITK_OVERRIDE
  {
    double x = m_Parameters[0];
    double y = m_Parameters[1];

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = -(3*x + 2*y -2);
    derivative[1] = -(2*x + 6*y +8);

    std::cout << "GetDerivative ( " << x << " , " << y << ") = " << "(" << -derivative[0] << " , " << -derivative[1] << ")" << std::endl;
  }

  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE
  {
    value = GetValue();
    GetDerivative( derivative );
  }

private:

  ParametersType  m_Parameters;
  bool            m_HasLocalSupport;

};

/** To ensure the events get fired. */
class EventChecker: public itk::Command
{
public:
  typedef EventChecker            Self;
  typedef itk::Command            Superclass;
  typedef itk::SmartPointer<Self> Pointer;

  itkNewMacro( Self );

  bool GetHadStartEvent()
    { return m_HadStartEvent; }
  bool GetHadIterationEvent()
    { return m_HadIterationEvent; }
  bool GetHadEndEvent()
    { return m_HadEndEvent; }

  virtual void Execute( itk::Object *caller, const itk::EventObject & event ) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  virtual void Execute( const itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    if( itk::StartEvent().CheckEvent( &event ))
      {
      std::cout << "Received StartEvent." << std::endl;
      m_HadStartEvent = true;
      }
    if( itk::IterationEvent().CheckEvent( &event ))
      {
      std::cout << "Received IterationEvent." << std::endl;
      const itk::ObjectToObjectOptimizerBaseTemplate<double> *opt =
        dynamic_cast<const itk::ObjectToObjectOptimizerBaseTemplate<double> *>(caller);
      if (opt)
        {
        std::cout << " iteration  = " << opt->GetCurrentIteration() << std::endl;
        }
      m_HadIterationEvent = true;
      }
    if( itk::EndEvent().CheckEvent( &event ))
      {
      std::cout << "Received EndEvent." << std::endl;
      m_HadEndEvent = true;
      }
    }

protected:
  EventChecker(): m_HadStartEvent( false ),
    m_HadIterationEvent( false ),
    m_HadEndEvent( false )
  {}

private:
  bool m_HadStartEvent;
  bool m_HadIterationEvent;
  bool m_HadEndEvent;
};


int itkLBFGSBOptimizerv4Test(int, char *[])
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  std::cout << "L-BFGS-B Optimizerv4 Test \n \n";

  typedef  itk::LBFGSBOptimizerv4  OptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // Declaration of the metric
  LBFGSBOptimizerv4TestMetric::Pointer metric = LBFGSBOptimizerv4TestMetric::New();

  itkOptimizer->SetMetric( metric.GetPointer() );

  const double F_Convergence_Factor  = 1e+7;      // Function value tolerance
  const double Projected_G_Tolerance = 1e-5;      // Proj gradient tolerance
  const int    Max_Iterations   =   25; // Maximum number of iterations

  itkOptimizer->SetCostFunctionConvergenceFactor( F_Convergence_Factor );
  itkOptimizer->SetGradientConvergenceTolerance( Projected_G_Tolerance );
  itkOptimizer->SetNumberOfIterations( Max_Iterations );
  itkOptimizer->SetMaximumNumberOfFunctionEvaluations( 100 );

  const unsigned int SpaceDimension = 2;
  OptimizerType::ParametersType initialValue(SpaceDimension);

  // Starting point
  initialValue[0] =  10;
  initialValue[1] =  10;

  // Set the initial position by setting the metric parameters.
  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters( initialValue );

  OptimizerType::ParametersType currentValue(2);

  currentValue = initialValue;

  itkOptimizer->SetInitialPosition( currentValue );

  /**
   * Set the boundary condition for each variable, where
   * select[i] = 0 if x[i] is unbounded,
   *           = 1 if x[i] has only a lower bound,
   *           = 2 if x[i] has both lower and upper bounds, and
   *           = 3 if x[1] has only an upper bound
   */
  OptimizerType::BoundValueType lower(SpaceDimension);
  OptimizerType::BoundValueType upper(SpaceDimension);
  OptimizerType::BoundSelectionType select(SpaceDimension);

  lower.Fill( -1 );
  upper.Fill( 10 );
  select.Fill( itk::LBFGSBOptimizerv4::BOTHBOUNDED );

  itkOptimizer->SetLowerBound( lower );
  itkOptimizer->SetUpperBound( upper );
  itkOptimizer->SetBoundSelection( select );

  itkOptimizer->Print( std::cout );

  EventChecker::Pointer eventChecker = EventChecker::New();
  itkOptimizer->AddObserver( itk::StartEvent(), eventChecker );
  itkOptimizer->AddObserver( itk::IterationEvent(), eventChecker );
  itkOptimizer->AddObserver( itk::EndEvent(), eventChecker );

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

  const OptimizerType::ParametersType & finalPosition = itkOptimizer->GetCurrentPosition();

  std::cout << "Solution = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;
  std::cout << "Final Function Value = " << itkOptimizer->GetValue() << std::endl;

  std::cout << "Infinity Norm of Projected Gradient = "
    << itkOptimizer->GetInfinityNormOfProjectedGradient() << std::endl;
  std::cout << "End condition   = "
    << itkOptimizer->GetStopConditionDescription() << std::endl;
  std::cout << "Trace   = " << itkOptimizer->GetTrace() << std::endl;
  std::cout << "metricConvergenceFactor   = "
    << itkOptimizer->GetCostFunctionConvergenceFactor() << std::endl;
  std::cout << "ProjectedGradientTolerance   = "
    << itkOptimizer->GetGradientConvergenceTolerance() << std::endl;
  std::cout << "NumberOfIterations   = "
    << itkOptimizer->GetNumberOfIterations() << std::endl;
  std::cout << "MaximumNumberOfEvaluations   = "
    << itkOptimizer->GetMaximumNumberOfFunctionEvaluations() << std::endl;
  std::cout << "MaximumNumberOfCorrections   = "
    << itkOptimizer->GetMaximumNumberOfCorrections() << std::endl;
  std::cout << "CurrentOfIterations  = " << itkOptimizer->GetCurrentIteration() << std::endl;

  if( !eventChecker->GetHadStartEvent() )
    {
    std::cerr << "Did not have StartEvent!" << std::endl;
    return EXIT_FAILURE;
    }
  if( !eventChecker->GetHadIterationEvent() )
    {
    std::cerr << "Did not have IterationEvent!" << std::endl;
    return EXIT_FAILURE;
    }
  if( !eventChecker->GetHadEndEvent() )
    {
    std::cerr << "Did not have EndEvent!" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // check results to see if it is within range
  //
  bool pass = true;
  std::string errorIn;

  // true parameters considering bounding constrains -1 <= x <= 10
  double trueParameters[2] = { 4.0/3.0, -1.0 };

  for( unsigned int j = 0; j < 2; ++j )
    {
    if( ! itk::Math::FloatAlmostEqual( finalPosition[j], trueParameters[j] ) )
      {
      pass = false;
      errorIn = "solution";
      }
    }

  if( ! itk::Math::FloatAlmostEqual( itkOptimizer->GetValue(), -7.66667, 4, 0.01 ) )
    {
    pass = false;
    errorIn = "final function value";
    }

  if( ! itk::Math::FloatAlmostEqual( itkOptimizer->GetInfinityNormOfProjectedGradient(), 1.77636e-15, 4, 0.01 ) )
    {
    pass = false;
    errorIn = "infinity norm of projected gradient";
    }

  if( !pass )
    {
    std::cerr << "\nError in " << errorIn << ".\n";
    std::cerr << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test stopping when number of iterations reached
  //
  std::cout << "-------------------------------" << std::endl;

  // Testing number of iterations for stopping
  metric->SetParameters( initialValue );
  itkOptimizer->SetNumberOfIterations( 1 );

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

   std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;
   std::cout << "NumberOfIterations  = " << itkOptimizer->GetCurrentIteration() << std::endl;

  if ( itkOptimizer->GetCurrentIteration() != 1 )
     {
     std::cout << "[FAILURE]" << std::endl;
     return EXIT_FAILURE;
     }


  //
  // Test with local-support transform. Should FAIL.
  // Such transforms are not yet supported.
  //
  std::cout << "-------------------------------" << std::endl;
  metric->SetHasLocalSupport( true );
  TRY_EXPECT_EXCEPTION( itkOptimizer->StartOptimization() );

  //
  //  Test in unbounded mode
  //
  std::cout << std::endl << "Test in unbounded mode:" << std::endl;

  OptimizerType::Pointer  itkOptimizer2 = OptimizerType::New();

  // Set up boundary conditions
  select.Fill( 0 );
  itkOptimizer2->SetBoundSelection( select );

  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters( initialValue );
  metric->SetHasLocalSupport( false );

  itkOptimizer2->SetMetric( metric.GetPointer() );
  itkOptimizer2->SetInitialPosition( currentValue );

  itkOptimizer2->SetCostFunctionConvergenceFactor( F_Convergence_Factor );
  itkOptimizer2->SetGradientConvergenceTolerance( Projected_G_Tolerance );
  itkOptimizer2->SetNumberOfIterations( Max_Iterations );
  itkOptimizer2->SetMaximumNumberOfFunctionEvaluations( Max_Iterations );

  itkOptimizer2->AddObserver( itk::StartEvent(), eventChecker );
  itkOptimizer2->AddObserver( itk::IterationEvent(), eventChecker );
  itkOptimizer2->AddObserver( itk::EndEvent(), eventChecker );

  try
    {
    itkOptimizer2->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation()    << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Boundaries after optimization: " << std::endl;
  std::cout << "Upper bound size: " << itkOptimizer2->GetUpperBound().size() << std::endl;
  std::cout << "Lower bound size: " << itkOptimizer2->GetLowerBound().size() << std::endl;

  const OptimizerType::ParametersType & finalPosition2 = itkOptimizer2->GetCurrentPosition();
  std::cout << "Solution = (" << finalPosition2[0] << "," << finalPosition2[1] << ")" << std::endl;
  std::cout << "Final Function Value = " << itkOptimizer2->GetValue() << std::endl;

  // check results
  pass = true;

  // true parameters when there is no constrain
  trueParameters[0] = 2.0;
  trueParameters[1] = -2.0;

  for( unsigned int j = 0; j < 2; ++j )
    {
    if( ! itk::Math::FloatAlmostEqual( finalPosition2[j], trueParameters[j], 4, 0.01 ) )
      {
      pass = false;
      errorIn = "solution";
      }
    }

  if( ! itk::Math::FloatAlmostEqual( itkOptimizer2->GetValue(), -10.0, 4, 0.01 ) )
    {
    pass = false;
    errorIn = "final function value";
    }

  if( !pass )
    {
    std::cerr << "\nError in " << errorIn << ".\n";
    std::cerr << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
