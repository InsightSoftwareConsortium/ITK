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

#include <set>
#include "itkLBFGS2Optimizerv4.h"
#include "itkMath.h"
#include "itkTestingMacros.h"
#include <iostream>

/**
 * \class itkLBFGSOptimizerv4TestMetric
 *
 *  The objectif function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is represented as an itkMatrix and
 *  b is represented as an itkVector
 *
 *  The system in this example is:
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *
 *   the solution is the vector | 2 -2 |
 *
 */
class itkLBFGS2Optimizerv4TestMetric : public itk::ObjectToObjectMetricBase
{
public:
  using Self = itkLBFGS2Optimizerv4TestMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);

  itkOverrideGetNameOfClassMacro(itkLBFGS2Optimizerv4TestMetric);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  itkLBFGS2Optimizerv4TestMetric() { m_HasLocalSupport = false; }

  MeasureType
  GetValue() const override
  {
    const double x = this->m_Parameters[0];
    const double y = this->m_Parameters[1];

    std::cout << "GetValue ( " << x << " , " << y << ") = ";

    const double val = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

    std::cout << val << '\n';

    return val;
  }

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    const double x = this->m_Parameters[0];
    const double y = this->m_Parameters[1];

    std::cout << "GetDerivative ( " << x << " , " << y << ") = ";

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = -(3 * x + 2 * y - 2);
    derivative[1] = -(2 * x + 6 * y + 8);

    std::cout << '(' << derivative[0] << " , " << derivative[1] << ')' << '\n';
  }

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

  Superclass::NumberOfParametersType
  GetNumberOfLocalParameters() const override
  {
    return SpaceDimension;
  }

  Superclass::NumberOfParametersType
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

  void
  SetParameters(ParametersType & params) override
  {
    this->m_Parameters = params;
  }

  const ParametersType &
  GetParameters() const override
  {
    return this->m_Parameters;
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
itkLBFGS2Optimizerv4Test(int, char *[])
{
  std::cout << "LBFGS2 Optimizerv4 Test \n \n";

  using OptimizerType = itk::LBFGS2Optimizerv4;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, LBFGS2Optimizerv4Template, GradientDescentOptimizerv4Template);


  // Declaration of the metric
  auto metric = itkLBFGS2Optimizerv4TestMetric::New();

  // Set some optimizer parameters
  auto hessianApproximationAccuracy = 5;
  itkOptimizer->SetHessianApproximationAccuracy(hessianApproximationAccuracy);
  ITK_TEST_SET_GET_VALUE(hessianApproximationAccuracy, itkOptimizer->GetHessianApproximationAccuracy());

  const typename OptimizerType::PrecisionType solutionAccuracy = 1e-5;
  itkOptimizer->SetSolutionAccuracy(solutionAccuracy);
  ITK_TEST_SET_GET_VALUE(solutionAccuracy, itkOptimizer->GetSolutionAccuracy());

  auto deltaConvergenceDistance = 0;
  itkOptimizer->SetDeltaConvergenceDistance(deltaConvergenceDistance);
  ITK_TEST_SET_GET_VALUE(deltaConvergenceDistance, itkOptimizer->GetDeltaConvergenceDistance());

  const typename OptimizerType::PrecisionType deltaConvergenceTolerance = 0;
  itkOptimizer->SetDeltaConvergenceTolerance(deltaConvergenceTolerance);
  ITK_TEST_SET_GET_VALUE(deltaConvergenceTolerance, itkOptimizer->GetDeltaConvergenceTolerance());

  auto maximumIterations = 0;
  itkOptimizer->SetMaximumIterations(maximumIterations);
  ITK_TEST_SET_GET_VALUE(maximumIterations, itkOptimizer->GetMaximumIterations());

  auto numberOfIterations = static_cast<itk::SizeValueType>(maximumIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, itkOptimizer->GetNumberOfIterations());

  itkOptimizer->SetNumberOfIterations(maximumIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, itkOptimizer->GetNumberOfIterations());

  const typename OptimizerType::LineSearchMethodEnum lineSearchMethod =
    OptimizerType::LineSearchMethodEnum::LINESEARCH_DEFAULT;
  itkOptimizer->SetLineSearch(lineSearchMethod);
  ITK_TEST_SET_GET_VALUE(lineSearchMethod, itkOptimizer->GetLineSearch());

  auto maximumLineSearchEvaluations = 20;
  itkOptimizer->SetMaximumLineSearchEvaluations(maximumLineSearchEvaluations);
  ITK_TEST_SET_GET_VALUE(maximumLineSearchEvaluations, itkOptimizer->GetMaximumLineSearchEvaluations());

  const typename OptimizerType::PrecisionType minimumLineSearchStep = 1e-20;
  itkOptimizer->SetMinimumLineSearchStep(minimumLineSearchStep);
  ITK_TEST_SET_GET_VALUE(minimumLineSearchStep, itkOptimizer->GetMinimumLineSearchStep());

  const typename OptimizerType::PrecisionType maximumLineSearchStep = 1e+20;
  itkOptimizer->SetMaximumLineSearchStep(maximumLineSearchStep);
  ITK_TEST_SET_GET_VALUE(maximumLineSearchStep, itkOptimizer->GetMaximumLineSearchStep());

  const typename OptimizerType::PrecisionType lineSearchAccuracy = 1e-4;
  itkOptimizer->SetLineSearchAccuracy(lineSearchAccuracy);
  ITK_TEST_SET_GET_VALUE(lineSearchAccuracy, itkOptimizer->GetLineSearchAccuracy());

  const typename OptimizerType::PrecisionType wolfeCoefficient = 0;
  itkOptimizer->SetWolfeCoefficient(wolfeCoefficient);
  ITK_TEST_SET_GET_VALUE(wolfeCoefficient, itkOptimizer->GetWolfeCoefficient());

  const typename OptimizerType::PrecisionType lineSearchGradientAccuracy = 0.9;
  itkOptimizer->SetLineSearchGradientAccuracy(lineSearchGradientAccuracy);
  ITK_TEST_SET_GET_VALUE(lineSearchGradientAccuracy, itkOptimizer->GetLineSearchGradientAccuracy());

  // itkOptimizer->SetMachinePrecisionTolerance():

  const typename OptimizerType::PrecisionType orthantwiseCoefficient = 0;
  itkOptimizer->SetOrthantwiseCoefficient(orthantwiseCoefficient);
  ITK_TEST_SET_GET_VALUE(orthantwiseCoefficient, itkOptimizer->GetOrthantwiseCoefficient());

  auto orthantwiseStart = 0;
  itkOptimizer->SetOrthantwiseStart(orthantwiseStart);
  ITK_TEST_SET_GET_VALUE(orthantwiseStart, itkOptimizer->GetOrthantwiseStart());

  auto orthantwiseEnd = 1;
  itkOptimizer->SetOrthantwiseEnd(orthantwiseEnd);
  ITK_TEST_SET_GET_VALUE(orthantwiseEnd, itkOptimizer->GetOrthantwiseEnd());

  auto estimateScalesAtEachIteration = true;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, EstimateScalesAtEachIteration, estimateScalesAtEachIteration);

  std::cout << "GetValue() before optimizer starts: ";
  std::cout << itkOptimizer->GetValue() << '\n';
  std::cout << "SetMetric." << '\n';
  itkOptimizer->SetMetric(metric);


  constexpr unsigned int        SpaceDimension = 2;
  OptimizerType::ParametersType initialValue(SpaceDimension);

  // We start not so far from  | 2 -2 |
  initialValue[0] = 100;
  initialValue[1] = -100;

  // Set the initial position by setting the metric
  // parameters.
  std::cout << "Set metric parameters." << '\n';
  metric->SetParameters(initialValue);

  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << '\n';

  std::cout << "Start optimization." << '\n';
  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << '\n';
    std::cerr << "An error occurred during Optimization" << '\n';
    std::cerr << "Location    = " << e.GetLocation() << '\n';
    std::cerr << "Description = " << e.GetDescription() << '\n';
    return EXIT_FAILURE;
  }


  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  std::cout << "Solution        = (" << finalPosition[0] << ',' << finalPosition[1] << ')' << '\n';

  std::cout << "End condition   = " << itkOptimizer->GetStopConditionDescription() << '\n';
  std::cout << "NumberOfIterations  = " << itkOptimizer->GetCurrentIteration() << '\n';

  std::cout << "CurrentParameterNorm: " << itkOptimizer->GetCurrentParameterNorm() << '\n';
  std::cout << "CurrentGradientNorm: " << itkOptimizer->GetCurrentGradientNorm() << '\n';
  std::cout << "CurrentStepSize: " << itkOptimizer->GetCurrentStepSize() << '\n';
  std::cout << "CurrentNumberOfEvaluations: " << itkOptimizer->GetCurrentNumberOfEvaluations() << '\n';

  //
  // check results to see if it is within range
  //
  bool         pass = true;
  const double trueParameters[2] = { 2, -2 };
  for (unsigned int j = 0; j < 2; ++j)
  {
    if (itk::Math::FloatAlmostEqual(finalPosition[j], trueParameters[j]))
    {
      pass = false;
    }
  }

  if (!pass)
  {
    std::cout << "Test failed." << '\n';
    return EXIT_FAILURE;
  }

  // Get the final value of the optimizer
  std::cout << "Testing GetValue() : ";
  const OptimizerType::MeasureType finalValue = itkOptimizer->GetValue();
  if (itk::Math::abs(finalValue + 10.0) > 0.01)
  {
    std::cout << "[FAILURE]" << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[SUCCESS]" << '\n';
  }

  //
  // Test stopping when number of iterations reached
  //
  maximumIterations = 5;
  itkOptimizer->SetMaximumIterations(maximumIterations);
  metric->SetParameters(initialValue);

  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << '\n';
    std::cerr << "An error occurred during Optimization" << '\n';
    std::cerr << e << '\n';
    return EXIT_FAILURE;
  }

  std::cout << "Solution        = (" << finalPosition[0] << ',' << finalPosition[1] << ')' << '\n';
  std::cout << "NumberOfIterations  = " << itkOptimizer->GetCurrentIteration() << '\n';

  if (itkOptimizer->GetCurrentIteration() != 2)
  {
    std::cout << "Not expected number of iterations!" << '\n';
    std::cout << "[FAILURE]" << '\n';
    return EXIT_FAILURE;
  }

  // Test streaming enumeration for LBFGS2Optimizerv4Enums::LineSearchMethod elements
  const std::set<itk::LBFGS2Optimizerv4Enums::LineSearchMethod> allLineSearchMethod{
    itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_DEFAULT,
    itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_MORETHUENTE,
    itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_ARMIJO,
    itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING,
    itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_WOLFE,
    itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_STRONG_WOLFE
  };
  for (const auto & ee : allLineSearchMethod)
  {
    std::cout << "STREAMED ENUM VALUE LBFGS2Optimizerv4Enums::LineSearchMethod: " << ee << '\n';
  }

  std::cout << "Test passed." << '\n';
  return EXIT_SUCCESS;
}
