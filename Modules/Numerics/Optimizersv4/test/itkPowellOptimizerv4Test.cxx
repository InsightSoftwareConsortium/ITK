/*=========================================================================
 *
 *  Copyright NumFOCUS
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
  using Self = PowellBoundedMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;


  PowellBoundedMetric() { m_HasLocalSupport = false; }

  MeasureType
  GetValue() const override
  {
    ++POWELL_CALLS_TO_GET_VALUE;

    double x = this->m_Parameters[0];
    double y = m_Parameters[1];

    std::cout << "      GetValue( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    MeasureType measure = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

    std::cout << measure << std::endl;

    return measure;
  }

  void
  GetDerivative(DerivativeType &) const override
  {}

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    value = GetValue();
    GetDerivative(derivative);
  }

  void
  Initialize() throw(itk::ExceptionObject) override
  {
    m_Parameters.SetSize(SpaceDimension);
  }

  unsigned int
  GetNumberOfLocalParameters() const override
  {
    return SpaceDimension;
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

  void
  SetParameters(ParametersType & parameters) override
  {
    m_Parameters = parameters;
  }

  const ParametersType &
  GetParameters() const override
  {
    return m_Parameters;
  }

  bool
  HasLocalSupport() const override
  {
    return m_HasLocalSupport;
  }

  void
  SetHasLocalSupport(bool hls)
  {
    m_HasLocalSupport = hls;
  }

  void
  UpdateTransformParameters(const DerivativeType &, ParametersValueType) override
  {}

private:
  ParametersType m_Parameters;
  bool           m_HasLocalSupport;
};


int
itkPowellOptimizerv4Test(int, char *[])
{
  std::cout << "PowellOptimizerv4 Test ";
  std::cout << std::endl << std::endl;

  using OptimizerType = itk::PowellOptimizerv4<double>;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction
  PowellBoundedMetric::Pointer metric = PowellBoundedMetric::New();


  itkOptimizer->SetMetric(metric);


  using ParametersType = PowellBoundedMetric::ParametersType;

  const unsigned int spaceDimension = metric->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType initialPosition(spaceDimension);

  initialPosition[0] = 100;
  initialPosition[1] = -100;

  // Set the initial position by setting the metric
  // parameters.
  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters(initialPosition);

  itkOptimizer->SetStepLength(10);
  itkOptimizer->SetStepTolerance(0.01);
  itkOptimizer->SetValueTolerance(0.1);
  itkOptimizer->SetMaximumIteration(100);

  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }

  ParametersType finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;
  std::cout << "StopConditionDescription: " << itkOptimizer->GetStopConditionDescription() << std::endl;

  //
  // check results to see if it is within range
  //
  bool   pass = true;
  double trueParameters[2] = { 2, -2 };
  for (unsigned int j = 0; j < 2; j++)
  {
    if (itk::Math::abs(finalPosition[j] - trueParameters[j]) > 0.01)
      pass = false;
  }

  // Exercise various member functions.
  std::cout << "StepLength: " << itkOptimizer->GetStepLength();
  std::cout << std::endl;
  std::cout << "CurrentIteration: " << itkOptimizer->GetCurrentIteration();
  std::cout << std::endl;

  itkOptimizer->Print(std::cout);

  std::cout << "Calls to GetValue = " << POWELL_CALLS_TO_GET_VALUE << std::endl;

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
