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

#include "itkPowellOptimizerv4.h"

int POWELL_CALLS_TO_GET_VALUE = 0;

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
 * \class PowellBoundedMetric
 *
 */
class PowellBoundedMetric : public itk::ObjectToObjectMetricBase
{
public:

  typedef PowellBoundedMetric             Self;
  typedef itk::ObjectToObjectMetricBase   Superclass;
  typedef itk::SmartPointer<Self>         Pointer;
  typedef itk::SmartPointer<const Self>   ConstPointer;
  itkNewMacro( Self );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType      ParametersType;
  typedef Superclass::DerivativeType      DerivativeType;
  typedef Superclass::MeasureType         MeasureType;


  PowellBoundedMetric()
  {
    m_HasLocalSupport = false;
  }

  virtual MeasureType  GetValue() const ITK_OVERRIDE
  {
    ++POWELL_CALLS_TO_GET_VALUE;

    double x = this->m_Parameters[0];
    double y = m_Parameters[1];

    std::cout << "      GetValue( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    MeasureType measure = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << measure << std::endl;

    return measure;
  }

  virtual void GetDerivative( DerivativeType & ) const ITK_OVERRIDE
  {
  }

  void GetValueAndDerivative( MeasureType & value,
                             DerivativeType & derivative ) const ITK_OVERRIDE
  {
    value = GetValue();
    GetDerivative( derivative );
  }

  virtual void Initialize(void) throw ( itk::ExceptionObject ) ITK_OVERRIDE
  {
    m_Parameters.SetSize( SpaceDimension );
  }

  virtual unsigned int GetNumberOfLocalParameters() const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
  {
    return SpaceDimension;
  }

  virtual void SetParameters( ParametersType & parameters ) ITK_OVERRIDE
  {
    m_Parameters = parameters;
  }

  virtual const ParametersType & GetParameters() const ITK_OVERRIDE
  {
    return m_Parameters;
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


int itkPowellOptimizerv4Test(int, char* [] )
{
  std::cout << "PowellOptimizerv4 Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::PowellOptimizerv4<double>  OptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction
  PowellBoundedMetric::Pointer metric = PowellBoundedMetric::New();


  itkOptimizer->SetMetric( metric.GetPointer() );


  typedef PowellBoundedMetric::ParametersType    ParametersType;

  const unsigned int spaceDimension =
                      metric->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition( spaceDimension );

  initialPosition[0] =  100;
  initialPosition[1] = -100;

  // Set the initial position by setting the metric
  // parameters.
  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters( initialPosition );

  itkOptimizer->SetStepLength( 10 );
  itkOptimizer->SetStepTolerance( 0.01 );
  itkOptimizer->SetValueTolerance( 0.1 );
  itkOptimizer->SetMaximumIteration( 100 );

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
  std::cout << "StopConditionDescription: "
            << itkOptimizer->GetStopConditionDescription() << std::endl;

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[2] = { 2, -2 };
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( itk::Math::abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      pass = false;
    }

  // Exercise various member functions.
  std::cout << "StepLength: " << itkOptimizer->GetStepLength();
  std::cout << std::endl;
  std::cout << "CurrentIteration: " << itkOptimizer->GetCurrentIteration();
  std::cout << std::endl;

  itkOptimizer->Print( std::cout );

  std::cout << "Calls to GetValue = " << POWELL_CALLS_TO_GET_VALUE << std::endl;

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}
