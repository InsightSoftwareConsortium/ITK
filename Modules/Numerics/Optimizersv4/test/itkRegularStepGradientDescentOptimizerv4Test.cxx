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
#include "itkGradientDescentOptimizerv4.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

/**
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

  enum { SpaceDimension = 2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::ParametersValueType ParametersValueType;
  typedef Superclass::MeasureType         MeasureType;


  RSGv4TestMetric()
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

    //
    // The optimizer simply takes the derivative from the metric
    // and adds it to the transform after scaling. So instead of
    // setting a 'minimize' option in the gradient, we return
    // a minimizing derivative.
    //
    derivative[0] = -( 3 * x + 2 * y -2 );
    derivative[1] = -( 2 * x + 6 * y +8 );

    std::cout << "derivative: " << derivative << std::endl;
  }

  virtual MeasureType  GetValue() const ITK_OVERRIDE
  {
    return 0.0;
  }

  virtual void UpdateTransformParameters( const DerivativeType & update, ParametersValueType factor ) ITK_OVERRIDE
  {
    m_Parameters += update * factor;
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

  // These Set/Get methods are only needed for this test derivation that
  // isn't using a transform.
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

template< typename OptimizerType >
int RegularStepGradientDescentOptimizerv4TestHelper(
  itk::SizeValueType numberOfIterations,
  bool doEstimateLearningRateAtEachIteration,
  bool doEstimateLearningRateOnce,
  typename OptimizerType::InternalComputationValueType relaxationFactor,
  typename OptimizerType::InternalComputationValueType minimumStepLength,
  typename OptimizerType::InternalComputationValueType gradientMagnitudeTolerance,
  typename OptimizerType::MeasureType currentLearningRateRelaxation )
{
  typedef typename OptimizerType::ScalesType ScalesType;

  typename OptimizerType::Pointer optimizer = OptimizerType::New();

  // Declaration of the metric
  RSGv4TestMetric::Pointer metric = RSGv4TestMetric::New();

  optimizer->SetMetric( metric );

  typedef RSGv4TestMetric::ParametersType ParametersType;

  const unsigned int spaceDimension =
    metric->GetNumberOfParameters();

  // We start not so far from | 2 -2 |
  ParametersType  initialPosition( spaceDimension );
  initialPosition[0] =  100;
  initialPosition[1] = -100;
  metric->SetParameters( initialPosition );

  ScalesType parametersScale( spaceDimension );
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  optimizer->SetScales( parametersScale );

  typename OptimizerType::InternalComputationValueType learningRate = 100;
  optimizer->SetLearningRate( learningRate );

  optimizer->SetNumberOfIterations( numberOfIterations );

  optimizer->SetDoEstimateLearningRateAtEachIteration( doEstimateLearningRateAtEachIteration );
  optimizer->SetDoEstimateLearningRateOnce( doEstimateLearningRateOnce );

  TEST_SET_GET_VALUE( doEstimateLearningRateAtEachIteration, optimizer->GetDoEstimateLearningRateAtEachIteration() );
  TEST_SET_GET_VALUE( doEstimateLearningRateOnce, optimizer->GetDoEstimateLearningRateOnce() );

  optimizer->SetRelaxationFactor( relaxationFactor );

  TEST_SET_GET_VALUE( relaxationFactor, optimizer->GetRelaxationFactor() );

  optimizer->SetMinimumStepLength( minimumStepLength );

  TEST_SET_GET_VALUE( minimumStepLength, optimizer->GetMinimumStepLength() );

  optimizer->SetGradientMagnitudeTolerance( gradientMagnitudeTolerance );

  TEST_SET_GET_VALUE( gradientMagnitudeTolerance,
    optimizer->GetGradientMagnitudeTolerance() );

  optimizer->SetCurrentLearningRateRelaxation( currentLearningRateRelaxation );

  TEST_SET_GET_VALUE( currentLearningRateRelaxation,
    optimizer->GetCurrentLearningRateRelaxation() );

  try
    {
    std::cout << "currentPosition before optimization: " << optimizer->GetMetric()->GetParameters() << std::endl;
    optimizer->StartOptimization();
    std::cout << "currentPosition after optimization: " << optimizer->GetMetric()->GetParameters() << std::endl;
    std::cout << " Stop Condition  = " << optimizer->GetStopConditionDescription() << std::endl;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  if( optimizer->GetCurrentIteration() > 0 )
    {
    std::cerr << "The optimizer is running iterations despite of ";
    std::cerr << "having a maximum number of iterations set to zero" << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType finalPosition = optimizer->GetMetric()->GetParameters();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;

  //
  // Check results to see if it is within range
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

  return EXIT_SUCCESS;
}


int itkRegularStepGradientDescentOptimizerv4Test( int, char* [] )
{
  std::cout << "RegularStepGradientDescentOptimizerv4 Test ";
  std::cout << std::endl << std::endl;

  typedef itk::RegularStepGradientDescentOptimizerv4< double > OptimizerType;

  OptimizerType::Pointer itkOptimizer = OptimizerType::New();

  EXERCISE_BASIC_OBJECT_METHODS( itkOptimizer, RegularStepGradientDescentOptimizerv4,
    GradientDescentOptimizerv4Template );

  bool testStatus = EXIT_SUCCESS;


  bool doEstimateLearningRateAtEachIteration = false;
  bool doEstimateLearningRateOnce = false;

  itk::SizeValueType numberOfIterations = 900;

  OptimizerType::InternalComputationValueType relaxationFactor = 0.5;
  OptimizerType::InternalComputationValueType minimumStepLength = 1e-6;
  OptimizerType::InternalComputationValueType gradientMagnitudeTolerance = 1e-6;
  OptimizerType::MeasureType currentLearningRateRelaxation = 0;

  testStatus = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations,
     doEstimateLearningRateAtEachIteration,
     doEstimateLearningRateOnce,
     relaxationFactor,
     minimumStepLength,
     gradientMagnitudeTolerance,
     currentLearningRateRelaxation );


  // Run now with different learning rate estimation frequencies
  std::cout << "\nRun test with a different learning rate estimation frequencies:"
    " estimate learning rate at each iteration: true; "
    " estimate learning rate once: false." << std::endl;
  {
  bool doEstimateLearningRateAtEachIteration2 = true;
  bool doEstimateLearningRateOnce2 = false;
  testStatus = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations,
     doEstimateLearningRateAtEachIteration2,
     doEstimateLearningRateOnce2,
     relaxationFactor,
     minimumStepLength,
     gradientMagnitudeTolerance,
     currentLearningRateRelaxation );
  }

  // Run now with different learning rate estimation frequencies
  std::cout << "\nRun test with a different learning rate estimation frequencies:"
    " estimate learning rate at each iteration: false; "
    " estimate learning rate once: true." << std::endl;
  {
  bool doEstimateLearningRateAtEachIteration3 = false;
  bool doEstimateLearningRateOnce3 = true;

  testStatus = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations,
     doEstimateLearningRateAtEachIteration3,
     doEstimateLearningRateOnce3,
     relaxationFactor,
     minimumStepLength,
     gradientMagnitudeTolerance,
     currentLearningRateRelaxation );
  }

  // Run now with different learning rate estimation frequencies
  std::cout << "\nRun test with a different learning rate estimation frequencies:"
    " estimate learning rate at each iteration: true; "
    " estimate learning rate once: true." << std::endl;
  {
  bool doEstimateLearningRateAtEachIteration4 = true;
  bool doEstimateLearningRateOnce4 = true;

  testStatus = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations,
     doEstimateLearningRateAtEachIteration4,
     doEstimateLearningRateOnce4,
     relaxationFactor,
     minimumStepLength,
     gradientMagnitudeTolerance,
     currentLearningRateRelaxation );
  }

  // Run now with a different relaxation factor
  std::cout << "\nRun test with a different relaxation factor: 0.8, instead of default value: 0.5." << std::endl;
  {
  OptimizerType::InternalComputationValueType relaxationFactor2 = 0.8;

  testStatus = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations,
     doEstimateLearningRateAtEachIteration,
     doEstimateLearningRateOnce,
     relaxationFactor2,
     minimumStepLength,
     gradientMagnitudeTolerance,
     currentLearningRateRelaxation );
  }


  // Verify that the optimizer doesn't run if the number of iterations is set to zero.
  std::cout << "\nCheck the optimizer when number of iterations is set to zero:" << std::endl;
  {
  itk::SizeValueType numberOfIterations2 = 0;

  testStatus = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations2,
     doEstimateLearningRateAtEachIteration,
     doEstimateLearningRateOnce,
     relaxationFactor,
     minimumStepLength,
     gradientMagnitudeTolerance,
     currentLearningRateRelaxation );
  }

  //
  // Test the Exception if the GradientMagnitudeTolerance is set to a negative value
  //
  std::cout << "\nTest the Exception if the GradientMagnitudeTolerance is set to a negative value:" << std::endl;
  {
  OptimizerType::InternalComputationValueType gradientMagnitudeTolerance2 = -1.0;
  bool expectedExceptionReceived = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations,
     doEstimateLearningRateAtEachIteration,
     doEstimateLearningRateOnce,
     relaxationFactor,
     minimumStepLength,
     gradientMagnitudeTolerance2,
     currentLearningRateRelaxation );

  if( !expectedExceptionReceived )
    {
    std::cerr << "Failure to produce an exception when";
    std::cerr << " the GradientMagnitudeTolerance is negative " << std::endl;
    std::cerr << "TEST FAILED !" << std::endl;
    testStatus = EXIT_FAILURE;
    }
  }

  //
  // Test the Exception if the RelaxationFactor is set to a value more than one.
  //
  std::cout << "\nTest the Exception if the RelaxationFactor is set to a value larger than one:" << std::endl;
  {
  itk::SizeValueType numberOfIterations3 = 100;
  OptimizerType::InternalComputationValueType relaxationFactor3 = 1.1;
  OptimizerType::InternalComputationValueType gradientMagnitudeTolerance3 = 0.01;
  bool expectedExceptionReceived = RegularStepGradientDescentOptimizerv4TestHelper< OptimizerType >(
     numberOfIterations3,
     doEstimateLearningRateAtEachIteration,
     doEstimateLearningRateOnce,
     relaxationFactor3,
     minimumStepLength,
     gradientMagnitudeTolerance3,
     currentLearningRateRelaxation );

  if( !expectedExceptionReceived )
    {
    std::cerr << "Failure to produce an exception when";
    std::cerr << " the RelaxationFactor is larger than one " << std::endl;
    std::cerr << "TEST FAILED !" << std::endl;
    testStatus = EXIT_FAILURE;
    }
  }

  if( !testStatus )
    {
    std::cout << "TEST FINISHED SUCCESSFULLY!" << std::endl;
    }
  else
    {
    std::cout << "TEST FAILED!" << std::endl;
    }

  return testStatus;

}
