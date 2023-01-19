/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkPowellOptimizerv4.h"
#include "itkTestingMacros.h"

int POWELL_CALLS_TO_GET_VALUE = 0;

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
  Initialize() override
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
itkPowellOptimizerv4Test(int argc, char * argv[])
{
  if (argc != 8)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " stepLength stepTolerance valueTolerance maximumIteration maximumLineIteration "
                 "catchGetValueException metricWorstPossibleValue"
              << std::endl;
    return EXIT_FAILURE;
  }

  using OptimizerType = itk::PowellOptimizerv4<double>;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, PowellOptimizerv4, ObjectToObjectOptimizerBaseTemplate);


  // Declaration of the CostFunction
  auto metric = PowellBoundedMetric::New();


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


  auto stepLength = std::stod(argv[1]);
  itkOptimizer->SetStepLength(stepLength);
  ITK_TEST_SET_GET_VALUE(stepLength, itkOptimizer->GetStepLength());

  auto stepTolerance = std::stod(argv[2]);
  itkOptimizer->SetStepTolerance(stepTolerance);
  ITK_TEST_SET_GET_VALUE(stepTolerance, itkOptimizer->GetStepTolerance());

  auto valueTolerance = std::stod(argv[3]);
  itkOptimizer->SetValueTolerance(valueTolerance);
  ITK_TEST_SET_GET_VALUE(valueTolerance, itkOptimizer->GetValueTolerance());

  auto maximumIteration = static_cast<unsigned int>(std::stoi(argv[4]));
  itkOptimizer->SetMaximumIteration(maximumIteration);
  ITK_TEST_SET_GET_VALUE(maximumIteration, itkOptimizer->GetMaximumIteration());

  auto maximumLineIteration = static_cast<unsigned int>(std::stoi(argv[5]));
  itkOptimizer->SetMaximumLineIteration(maximumLineIteration);
  ITK_TEST_SET_GET_VALUE(maximumLineIteration, itkOptimizer->GetMaximumLineIteration());

  auto catchGetValueException = static_cast<bool>(std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, CatchGetValueException, catchGetValueException);

  auto metricWorstPossibleValue = std::stod(argv[7]);
  itkOptimizer->SetMetricWorstPossibleValue(metricWorstPossibleValue);
  ITK_TEST_SET_GET_VALUE(metricWorstPossibleValue, itkOptimizer->GetMetricWorstPossibleValue());

  ITK_TRY_EXPECT_NO_EXCEPTION(itkOptimizer->StartOptimization());


  ParametersType finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;
  std::cout << "StopConditionDescription: " << itkOptimizer->GetStopConditionDescription() << std::endl;

  //
  // check results to see if it is within range
  //
  bool   pass = true;
  double trueParameters[2] = { 2, -2 };
  for (unsigned int j = 0; j < 2; ++j)
  {
    if (itk::Math::abs(finalPosition[j] - trueParameters[j]) > 0.01)
    {
      pass = false;
    }
  }

  // Exercise various member functions.
  std::cout << "CurrentIteration: " << itkOptimizer->GetCurrentIteration() << std::endl;
  std::cout << "CurrentCost: " << itkOptimizer->GetCurrentCost() << std::endl;
  std::cout << "CurrentCost (through GetValue): " << itkOptimizer->GetValue() << std::endl;
  std::cout << "CurrentLineIteration: " << itkOptimizer->GetCurrentLineIteration() << std::endl;

  std::cout << "Calls to GetValue = " << POWELL_CALLS_TO_GET_VALUE << std::endl;

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
