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

/* This test simulates the use of a metric with a transform
 * with local support, testing the proper handling of scales for such a case.
 *
 * Cribbed originally from itkGradientDescentOptimizerTest */

/**
 *  \class GradientDescentOptimizerv4Test2Metric for test
 *
 *  The version for this test returns a derivative that simulates
 *  the return from a metric working with a transform with local support.
 *  The derivative does not change, it's only meant to test the mechanics
 *  of applying scales in one iteration of optimization.
 *
 */
class GradientDescentOptimizerv4Test2Metric
  : public itk::ObjectToObjectMetricBase
{
public:

  typedef GradientDescentOptimizerv4Test2Metric Self;
  typedef itk::ObjectToObjectMetricBase         Superclass;
  typedef itk::SmartPointer<Self>               Pointer;
  typedef itk::SmartPointer<const Self>         ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( GradientDescentOptimizerv4Test2Metric, ObjectToObjectMetricBase );

  enum { SpaceDimension=3 };

  typedef Superclass::ParametersType          ParametersType;
  typedef Superclass::ParametersValueType     ParametersValueType;
  typedef Superclass::NumberOfParametersType  NumberOfParametersType;
  typedef Superclass::DerivativeType          DerivativeType;
  typedef Superclass::MeasureType             MeasureType;

  GradientDescentOptimizerv4Test2Metric()
  {
    m_Parameters.SetSize( this->GetNumberOfParameters() );
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
    if( derivative.Size() != this->GetNumberOfParameters() )
      derivative.SetSize( this->GetNumberOfParameters() );

    value = 0.0;

    for( NumberOfParametersType i=0; i < this->GetNumberOfParameters(); i++ )
      {
      derivative[i] = i;
      }

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
    return SpaceDimension * 3;
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
int itkGradientDescentOptimizerv4Test2(int, char* [] )
{
  std::cout << "Gradient Descent Object Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::GradientDescentOptimizerv4  OptimizerType;

  typedef OptimizerType::ScalesType             ScalesType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();

  // Declaration of the Metric
  GradientDescentOptimizerv4Test2Metric::Pointer metric = GradientDescentOptimizerv4Test2Metric::New();

  itkOptimizer->SetMetric( metric );

  typedef GradientDescentOptimizerv4Test2Metric::ParametersType    ParametersType;
  typedef GradientDescentOptimizerv4Test2Metric::NumberOfParametersType    NumberOfParametersType;

  ParametersType  initialPosition( metric->GetNumberOfParameters() );
  initialPosition.Fill(0);
  metric->SetParameters( initialPosition );

  itkOptimizer->SetLearningRate( 1.0 );
  itkOptimizer->SetNumberOfIterations( 1 );

  ScalesType scales( metric->GetNumberOfLocalParameters() );
  for( NumberOfParametersType i=0; i < metric->GetNumberOfLocalParameters(); i++ )
    {
    scales[i] = i+2;
    }
  itkOptimizer->SetScales( scales );

  ParametersType truth( metric->GetNumberOfParameters() );
  NumberOfParametersType numLocal = metric->GetNumberOfLocalParameters();
  NumberOfParametersType numLoops = metric->GetNumberOfParameters() / numLocal;
  for( NumberOfParametersType i=0; i < numLoops; i++ )
    {
    for( NumberOfParametersType j=0; j < numLocal; j++ )
      {
      NumberOfParametersType ind = i * numLocal + j;
      truth[ind] = initialPosition[ind] + (ind) / scales[j];
      }
    }
  std::cout << "truth: " << truth << std::endl;

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

  ParametersType finalPosition = metric->GetParameters();
  std::cout << "finalPosition = " << finalPosition << std::endl;

  //
  // check results to see if it is within range
  //
  for( NumberOfParametersType j = 0; j < metric->GetNumberOfParameters(); j++ )
    {
    if( itk::Math::abs( finalPosition[j] - truth[j] ) > 0.000001 )
      {
      std::cerr << "Results do not match: " << std::endl
                << "expected: " << truth << std::endl
                << "actual: " << finalPosition << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
