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
#include "itkRegistrationParameterScalesFromShift.h"

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
  : public itk::ObjectToObjectMetric
{
public:

  typedef GradientDescentOptimizerv4TestMetric      Self;
  typedef itk::ObjectToObjectMetric                 Superclass;
  typedef itk::SmartPointer<Self>                   Pointer;
  typedef itk::SmartPointer<const Self>             ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( GradientDescentOptimizerv4TestMetric, ObjectToObjectMetric );

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

  void Initialize(void) throw ( itk::ExceptionObject ) {}

  void GetValueAndDerivative( MeasureType & value,
                              DerivativeType & derivative ) const
  {
    if( derivative.Size() != 2 )
      derivative.SetSize(2);

    double x = m_Parameters[0];
    double y = m_Parameters[1];

    std::cout << "GetValueAndDerivative( ";
    std::cout << x << " ";
    std::cout << y << ") = " << std::endl;

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

  void UpdateTransformParameters( DerivativeType & update, ParametersValueType )
  {
    m_Parameters += update;
  }

  unsigned int GetNumberOfParameters(void) const
  {
    return SpaceDimension;
  }

  unsigned int GetNumberOfLocalParameters() const
  {
    return SpaceDimension;
  }

  bool HasLocalSupport() const
  {
    return false;
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
int GradientDescentOptimizerv4RunTest(
  itk::GradientDescentOptimizerv4::Pointer & itkOptimizer )
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
    std::cout << "An error ocurred during Optimization" << std::endl;
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
  double trueParameters[2] = { 2, -2 };
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( vnl_math_abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
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

  const unsigned int spaceDimension =
                      metric->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;
  metric->SetParameters( initialPosition );

  itkOptimizer->SetLearningRate( 0.1 );
  itkOptimizer->SetNumberOfIterations( 50 );

  // test the optimization
  std::cout << "Test optimization 1:" << std::endl;
  if( GradientDescentOptimizerv4RunTest( itkOptimizer ) == EXIT_FAILURE )
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
  if( GradientDescentOptimizerv4RunTest( itkOptimizer ) == EXIT_FAILURE )
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
