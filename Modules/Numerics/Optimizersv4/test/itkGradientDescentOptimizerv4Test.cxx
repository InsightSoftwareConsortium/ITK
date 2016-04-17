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
#include "itkGradientDescentOptimizerv4.h"
#include "itkTestingMacros.h"

/* Cribbed from itkGradientDescentOptimizerTest */

/**
 *  \class GradientDescentOptimizerv4TestMetric for test
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
class GradientDescentOptimizerv4TestMetric
  : public itk::ObjectToObjectMetricBase
{
public:

  typedef GradientDescentOptimizerv4TestMetric      Self;
  typedef itk::ObjectToObjectMetricBase             Superclass;
  typedef itk::SmartPointer<Self>                   Pointer;
  typedef itk::SmartPointer<const Self>             ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( GradientDescentOptimizerv4TestMetric, ObjectToObjectMetricBase );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType        ParametersType;
  typedef Superclass::ParametersValueType   ParametersValueType;
  typedef Superclass::DerivativeType        DerivativeType;
  typedef Superclass::MeasureType           MeasureType;

  GradientDescentOptimizerv4TestMetric()
  {
    m_Parameters.SetSize( SpaceDimension );
    m_Parameters.Fill( 0 );
  }

  virtual void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE {}

  virtual void GetDerivative( DerivativeType & derivative ) const ITK_OVERRIDE
  {
    MeasureType value;
    GetValueAndDerivative( value, derivative );
  }

  void GetValueAndDerivative( MeasureType & value,
                              DerivativeType & derivative ) const ITK_OVERRIDE
  {
    if( derivative.Size() != 2 )
      derivative.SetSize(2);

    double x = m_Parameters[0];
    double y = m_Parameters[1];

    std::cout << "GetValueAndDerivative( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    value = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << "value: " << value << std::endl;

    /* The optimizer simply takes the derivative from the metric
     * and adds it to the transform after scaling. So instead of
     * setting a 'minimize' option in the gradient, we return
     * a minimizing derivative. */
    derivative[0] = -( 3 * x + 2 * y -2 );
    derivative[1] = -( 2 * x + 6 * y +8 );

    std::cout << "derivative: " << derivative << std::endl;
  }

  virtual MeasureType  GetValue() const ITK_OVERRIDE
  {
    return 0.0;
  }

  virtual void UpdateTransformParameters( const DerivativeType & update, ParametersValueType ) ITK_OVERRIDE
  {
    m_Parameters += update;
  }

  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  virtual bool HasLocalSupport() const ITK_OVERRIDE
    {
    return false;
    }

  virtual unsigned int GetNumberOfLocalParameters() const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  /* These Set/Get methods are only needed for this test derivation that
   * isn't using a transform */
  virtual void SetParameters( ParametersType & parameters ) ITK_OVERRIDE
  {
    m_Parameters = parameters;
  }

  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
  {
    return m_Parameters;
  }

private:

  ParametersType m_Parameters;
};

///////////////////////////////////////////////////////////
int GradientDescentOptimizerv4RunTest( itk::GradientDescentOptimizerv4::Pointer & itkOptimizer,
                                       GradientDescentOptimizerv4TestMetric::ParametersType & trueParameters )
{
  try
    {
    std::cout << "currentPosition before optimization: " << itkOptimizer->GetCurrentPosition() << std::endl;
    itkOptimizer->StartOptimization();
    std::cout << "currentPosition after optimization: " << itkOptimizer->GetCurrentPosition() << std::endl;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  typedef GradientDescentOptimizerv4TestMetric::ParametersType    ParametersType;
  ParametersType finalPosition = itkOptimizer->GetMetric()->GetParameters();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;

  //
  // check results to see if it is within range
  //
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( itk::Math::abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      {
      std::cerr << "Results do not match: " << std::endl
                << "expected: " << trueParameters << std::endl
                << "returned: " << finalPosition << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
///////////////////////////////////////////////////////////
int itkGradientDescentOptimizerv4Test(int, char* [] )
{
  std::cout << "Gradient Descent Object Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::GradientDescentOptimizerv4  OptimizerType;

  typedef OptimizerType::ScalesType             ScalesType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // Declaration of the Metric
  GradientDescentOptimizerv4TestMetric::Pointer metric = GradientDescentOptimizerv4TestMetric::New();

  itkOptimizer->SetMetric( metric );

  typedef GradientDescentOptimizerv4TestMetric::ParametersType    ParametersType;

  const unsigned int spaceDimension = metric->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;
  metric->SetParameters( initialPosition );

  itkOptimizer->SetLearningRate( 0.1 );
  itkOptimizer->SetNumberOfIterations( 50 );

  // Truth
  ParametersType trueParameters(2);
  trueParameters[0] = 2;
  trueParameters[1] = -2;

  // test the optimization
  std::cout << "Test optimization 1:" << std::endl;
  if( GradientDescentOptimizerv4RunTest( itkOptimizer, trueParameters ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  //
  // test with non-idenity scales
  //
  std::cout << "Test optimization with non-identity scales:" << std::endl;
  ScalesType scales( metric->GetNumberOfLocalParameters() );
  scales.Fill(0.5);
  itkOptimizer->SetScales( scales );
  metric->SetParameters( initialPosition );
  if( GradientDescentOptimizerv4RunTest( itkOptimizer, trueParameters ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  // test with weights {0.4,0.5}
  std::cout << std::endl << "Test with weights {0.4,0.5}:" << std::endl;
  scales.Fill(1.0);
  itkOptimizer->SetScales( scales );
  ScalesType weights(2);
  weights[0] = 0.4;
  weights[1] = 0.5;
  itkOptimizer->SetWeights( weights );
  itkOptimizer->SetNumberOfIterations( 110 );
  metric->SetParameters( initialPosition );
  if( GradientDescentOptimizerv4RunTest( itkOptimizer, trueParameters ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  // test with weights {0,1}
  std::cout << std::endl << "Test with weights {0,1}:" << std::endl;
  scales.Fill(1.0);
  itkOptimizer->SetScales( scales );
  weights[0] = 0.0;
  weights[1] = 1.0;
  itkOptimizer->SetWeights( weights );
  trueParameters[0] = initialPosition[0];
  trueParameters[1] = -34.6667;
  metric->SetParameters( initialPosition );
  itkOptimizer->SetNumberOfIterations( 40 );
  if( GradientDescentOptimizerv4RunTest( itkOptimizer, trueParameters ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  // test with weights {1,0}
  std::cout << std::endl << "Test with weights {1,0}:" << std::endl;
  scales.Fill(1.0);
  itkOptimizer->SetScales( scales );
  weights[0] = 1.0;
  weights[1] = 0.0;
  itkOptimizer->SetWeights( weights );
  trueParameters[0] = 67.3333;
  trueParameters[1] = initialPosition[1];
  metric->SetParameters( initialPosition );
  if( GradientDescentOptimizerv4RunTest( itkOptimizer, trueParameters ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  // Exercise various member functions.
  std::cout << "LearningRate: " << itkOptimizer->GetLearningRate();
  std::cout << std::endl;
  std::cout << "NumberOfIterations: " << itkOptimizer->GetNumberOfIterations();
  std::cout << std::endl;
  // For test of learning rate and scales estimation options
  // in an actual registration, see
  // itkAutoScaledGradientDescentRegistrationTest.
  itkOptimizer->SetDoEstimateLearningRateOnce( false );
  std::cout << "GetDoEstimateLearningRateOnce: " << itkOptimizer->GetDoEstimateLearningRateOnce() << std::endl;
  itkOptimizer->SetDoEstimateLearningRateAtEachIteration( true );
  std::cout << "GetDoEstimateLearningRateAtEachIteration: " << itkOptimizer->GetDoEstimateLearningRateAtEachIteration() << std::endl;
  itkOptimizer->SetDoEstimateScales( false );
  std::cout << "GetDoEstimateScales: " << itkOptimizer->GetDoEstimateScales() << std::endl;

  itkOptimizer->Print( std::cout );
  std::cout << "Stop description   = "
            << itkOptimizer->GetStopConditionDescription() << std::endl;

  //
  // Verify that the optimizer doesn't run if the
  // number of iterations is set to zero.
  //
  std::cout << "\nCheck the optimizer when number of iterations is set to zero:" << std::endl;
  {
  itkOptimizer->SetNumberOfIterations( 0 );
  metric->SetParameters( initialPosition );
  trueParameters[0] = 100;
  trueParameters[1] = -100;
  if( GradientDescentOptimizerv4RunTest( itkOptimizer, trueParameters ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkOptimizer->GetCurrentIteration() > 0 )
    {
    std::cout << "The optimizer is running iterations despite of ";
    std::cout << "having a maximum number of iterations set to zero" << std::endl;
    return EXIT_FAILURE;
    }
  }

  std::cout << "\nTest the Exception if the optimizer is not set properly:" << std::endl;
  OptimizerType::Pointer badOptimizer = OptimizerType::New();
  bool caught=false;
  try
    {
    badOptimizer->GetCurrentPosition();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Caught expected exception!";
    std::cout << e << std::endl;
    caught = true;
    }

  if (!caught)
    {
    std::cout << "Failed to catch expected exception! " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout <<  "Printing self.. " << std::endl;
  std::cout << itkOptimizer  << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
