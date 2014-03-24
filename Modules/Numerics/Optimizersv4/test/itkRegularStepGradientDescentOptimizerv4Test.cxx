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

#include "itkRegularStepGradientDescentOptimizerv4.h"
#include "vnl/vnl_math.h"

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
 * \class RSGv4TestMetric
 */
class RSGv4TestMetric : public itk::ObjectToObjectMetricBase
{
public:

  typedef RSGv4TestMetric               Self;
  typedef itk::ObjectToObjectMetricBase Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::ParametersValueType ParametersValueType;
  typedef Superclass::MeasureType         MeasureType;


  RSGv4TestMetric()
  {
    m_Parameters.SetSize( SpaceDimension );
    m_Parameters.Fill( 0 );
  }

  void Initialize(void) throw ( itk::ExceptionObject ) {}

  void GetDerivative( DerivativeType & derivative ) const
  {
    MeasureType value;
    GetValueAndDerivative( value, derivative );
  }

  void GetValueAndDerivative( MeasureType & value,
                              DerivativeType & derivative ) const
  {
    if( derivative.Size() != 2 )
      {
      derivative.SetSize(2);
      }

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

  MeasureType  GetValue() const
  {
    return 0.0;
  }

  void UpdateTransformParameters( const DerivativeType & update, ParametersValueType factor )
  {
    m_Parameters += update * factor;
  }

  unsigned int GetNumberOfParameters(void) const
  {
    return SpaceDimension;
  }

  virtual bool HasLocalSupport() const
  {
    return false;
  }

  unsigned int GetNumberOfLocalParameters() const
  {
    return SpaceDimension;
  }

  /* These Set/Get methods are only needed for this test derivation that
   * isn't using a transform */
  void SetParameters( ParametersType & parameters )
  {
    m_Parameters = parameters;
  }

  const ParametersType & GetParameters() const
  {
    return m_Parameters;
  }

private:
  ParametersType m_Parameters;
};

///////////////////////////////////////////////////////////
int itkRegularStepGradientDescentOptimizerv4Test(int, char* [] )
{
  std::cout << "RegularStepGradientDescentOptimizerv4 Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::RegularStepGradientDescentOptimizerv4<double>  OptimizerType;

  typedef  OptimizerType::ScalesType  ScalesType;


  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the metric
  RSGv4TestMetric::Pointer metric = RSGv4TestMetric::New();


  itkOptimizer->SetMetric( metric );


  typedef RSGv4TestMetric::ParametersType    ParametersType;


  const unsigned int spaceDimension =
                      metric->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );
  initialPosition[0] =  100;
  initialPosition[1] = -100;
  metric->SetParameters( initialPosition );

  ScalesType    parametersScale( spaceDimension );
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  itkOptimizer->SetScales( parametersScale );
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-6 );
  itkOptimizer->SetLearningRate( 100 );
  itkOptimizer->SetMinimumStepLength( 1e-6 );
  itkOptimizer->SetNumberOfIterations( 900 );

  try
    {
    std::cout << "currentPosition before optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    itkOptimizer->StartOptimization();
    std::cout << "currentPosition after optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    std::cout << " Stop Condition  = " << itkOptimizer->GetStopConditionDescription() << std::endl;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }


  ParametersType finalPosition = itkOptimizer->GetMetric()->GetParameters();
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


  // Run now with a different relaxation factor
  std::cout << "\nRun test with a different relaxation factor: 0.8, instead of default value: 0.5." << std::endl;
  {
  metric->SetParameters( initialPosition );

  itkOptimizer->SetRelaxationFactor( 0.8 );
  try
    {
    std::cout << "currentPosition before optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    itkOptimizer->StartOptimization();
    std::cout << "currentPosition after optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    std::cout << " Stop Condition  = " << itkOptimizer->GetStopConditionDescription() << std::endl;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  finalPosition = itkOptimizer->GetMetric()->GetParameters();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;

  //
  // check results to see if it is within range
  //
  pass = true;
  for( unsigned int j = 0; j < 2; j++ )
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

  }

  //
  // Verify that the optimizer doesn't run if the
  // number of iterations is set to zero.
  //
  std::cout << "\nCheck the optimizer when number of iterations is set to zero:" << std::endl;
  {
  itkOptimizer->SetNumberOfIterations( 0 );
  metric->SetParameters( initialPosition );

  try
    {
    std::cout << "currentPosition before optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    itkOptimizer->StartOptimization();
    std::cout << "currentPosition after optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    std::cout << " Stop Condition  = " << itkOptimizer->GetStopConditionDescription() << std::endl;
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
  std::cout << "\nTest the Exception if the GradientMagnitudeTolerance is set to a negative value:" << std::endl;
  itkOptimizer->SetGradientMagnitudeTolerance( -1.0 );
  bool expectedExceptionReceived = false;
  try
    {
    std::cout << "currentPosition before optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    itkOptimizer->StartOptimization();
    std::cout << "currentPosition after optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
    std::cout << " Stop Condition  = " << itkOptimizer->GetStopConditionDescription() << std::endl;
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

  //
  // Test the Exception if the RelaxationFactor is set to a value more than one.
  //
  std::cout << "\nTest the Exception if the RelaxationFactor is set to a value more than one:" << std::endl;
  itkOptimizer->SetNumberOfIterations( 100 );
  itkOptimizer->SetGradientMagnitudeTolerance( 0.01 );
  itkOptimizer->SetRelaxationFactor( 1.1 );
  expectedExceptionReceived = false;
  try
  {
  std::cout << "currentPosition before optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
  itkOptimizer->StartOptimization();
  std::cout << "currentPosition after optimization: " << itkOptimizer->GetMetric()->GetParameters() << std::endl;
  std::cout << " Stop Condition  = " << itkOptimizer->GetStopConditionDescription() << std::endl;
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
    std::cerr << "the RelaxationFactor is more than one " << std::endl;
    std::cerr << "TEST FAILED !" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "TEST PASSED !" << std::endl;
  return EXIT_SUCCESS;

}
