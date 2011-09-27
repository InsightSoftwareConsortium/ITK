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
#include "itkObjectToObjectMetric.h"
#include "itkGradientDescentObjectOptimizer.h"
#include "vnl/vnl_math.h"

/* Cribbed from itkGradientDescentOptimizerTest */

/**
 *  \class GradientDescentObjectOptimizerTestMetric for test
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
class GradientDescentObjectOptimizerTestMetric
  : public itk::ObjectToObjectMetric
{
public:

  typedef GradientDescentObjectOptimizerTestMetric  Self;
  typedef itk::ObjectToObjectMetric                 Superclass;
  typedef itk::SmartPointer<Self>                   Pointer;
  typedef itk::SmartPointer<const Self>             ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( GradientDescentObjectOptimizerTestMetric, ObjectToObjectMetric );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType        ParametersType;
  typedef Superclass::ParametersValueType   ParametersValueType;
  typedef Superclass::DerivativeType        DerivativeType;
  typedef Superclass::MeasureType           MeasureType;

  GradientDescentObjectOptimizerTestMetric()
  {
    m_Parameters.SetSize( SpaceDimension );
    m_Parameters.Fill( 0 );
  }

  void Initialize(void) throw ( itk::ExceptionObject ) {}

  using Superclass::GetValueAndDerivative;
  void GetValueAndDerivative( MeasureType & value,
                              DerivativeType & derivative )
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

  using Superclass::GetValue;
  MeasureType  GetValue()
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

int itkGradientDescentObjectOptimizerTest(int, char* [] )
{
  std::cout << "Gradient Descent Object Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::GradientDescentObjectOptimizer  OptimizerType;

  typedef OptimizerType::ScalesType             ScalesType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // Declaration of the Metric
  GradientDescentObjectOptimizerTestMetric::Pointer metric = GradientDescentObjectOptimizerTestMetric::New();

  itkOptimizer->SetMetric( metric );

  typedef GradientDescentObjectOptimizerTestMetric::ParametersType    ParametersType;

  const unsigned int spaceDimension =
                      metric->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;
  metric->SetParameters( initialPosition );

  itkOptimizer->SetLearningRate( 0.1 );
  itkOptimizer->SetNumberOfIterations( 50 );

  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType finalPosition = metric->GetParameters();
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
    if( vnl_math_abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      {
      pass = false;
      }
    }

  // Exercise various member functions.
  std::cout << "LearningRate: " << itkOptimizer->GetLearningRate();
  std::cout << std::endl;
  std::cout << "NumberOfIterations: " << itkOptimizer->GetNumberOfIterations();
  std::cout << std::endl;

  itkOptimizer->Print( std::cout );
  std::cout << "Stop description   = "
            << itkOptimizer->GetStopConditionDescription() << std::endl;

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout <<  "Printing self.. " << std::endl;
  std::cout << itkOptimizer  << std::endl;


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}
