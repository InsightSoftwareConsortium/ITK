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
 *  b is represented as a itkVector
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

  itkTypeMacro(itkLBFGS2Optimizerv4TestMetric, ObjectToObjectMetricBase);

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
    double x = this->m_Parameters[0];
    double y = this->m_Parameters[1];

    std::cout << "GetValue ( " << x << " , " << y << ") = ";

    double val = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

    std::cout << val << std::endl;

    return val;
  }

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    double x = this->m_Parameters[0];
    double y = this->m_Parameters[1];

    std::cout << "GetDerivative ( " << x << " , " << y << ") = ";

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = -(3 * x + 2 * y - 2);
    derivative[1] = -(2 * x + 6 * y + 8);

    std::cout << "(" << derivative[0] << " , " << derivative[1] << ")" << std::endl;
  }

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

  // Declaration of a itkOptimizer
  OptimizerType::Pointer itkOptimizer = OptimizerType::New();

  // Declaration of the metric
  itkLBFGS2Optimizerv4TestMetric::Pointer metric = itkLBFGS2Optimizerv4TestMetric::New();

  // Set some optimizer parameters
  itkOptimizer->SetHessianApproximationAccuracy(5);
  itkOptimizer->SetSolutionAccuracy(1e-5);
  itkOptimizer->SetDeltaConvergenceDistance(0);
  itkOptimizer->SetDeltaConvergenceTolerance(0);
  itkOptimizer->SetMaximumIterations(0);
  itkOptimizer->SetLineSearch(OptimizerType::LineSearchMethodEnum::LINESEARCH_DEFAULT);
  itkOptimizer->SetMaximumLineSearchEvaluations(20);
  itkOptimizer->SetMinimumLineSearchStep(1e-20);
  itkOptimizer->SetMaximumLineSearchStep(1e+20);
  itkOptimizer->SetLineSearchAccuracy(1e-4);
  itkOptimizer->SetWolfeCoefficient(0);
  itkOptimizer->SetLineSearchGradientAccuracy(0.9);
  // itkOptimizer->SetMachinePrecisionTolerance():
  itkOptimizer->SetOrthantwiseCoefficient(0);
  itkOptimizer->SetOrthantwiseStart(0);
  itkOptimizer->SetOrthantwiseEnd(1);


  std::cout << "GetValue() before optimizer starts: " << itkOptimizer->GetValue() << std::endl;
  std::cout << "SetMetric." << std::endl;
  itkOptimizer->SetMetric(metric);


  constexpr unsigned int        SpaceDimension = 2;
  OptimizerType::ParametersType initialValue(SpaceDimension);

  // We start not so far from  | 2 -2 |
  initialValue[0] = 100;
  initialValue[1] = -100;

  // Set the initial position by setting the metric
  // parameters.
  std::cout << "Set metric parameters." << std::endl;
  metric->SetParameters(initialValue);

  itkOptimizer->Print(std::cout);
  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

  std::cout << "Start optimization." << std::endl;
  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << "Location    = " << e.GetLocation() << std::endl;
    std::cerr << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }


  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;

  std::cout << "End condition   = " << itkOptimizer->GetStopConditionDescription() << std::endl;
  std::cout << "LineSearchAccuracy   = " << itkOptimizer->GetLineSearchAccuracy() << std::endl;
  std::cout << "SolutionAccuracy   = " << itkOptimizer->GetSolutionAccuracy() << std::endl;
  std::cout << "NumberOfIterations  = " << itkOptimizer->GetCurrentIteration() << std::endl;

  //
  // check results to see if it is within range
  //
  bool   pass = true;
  double trueParameters[2] = { 2, -2 };
  for (unsigned int j = 0; j < 2; ++j)
  {
    if (itk::Math::FloatAlmostEqual(finalPosition[j], trueParameters[j]))
    {
      pass = false;
    }
  }

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  // Get the final value of the optimizer
  std::cout << "Testing GetValue() : ";
  OptimizerType::MeasureType finalValue = itkOptimizer->GetValue();
  if (std::fabs(finalValue + 10.0) > 0.01)
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[SUCCESS]" << std::endl;
  }

  //
  // Test stopping when number of iterations reached
  //
  itkOptimizer->SetMaximumIterations(5);
  metric->SetParameters(initialValue);

  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during Optimization" << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;
  std::cout << "NumberOfIterations  = " << itkOptimizer->GetCurrentIteration() << std::endl;

  if (itkOptimizer->GetCurrentIteration() != 5)
  {
    std::cout << "Not expected number of iterations!" << std::endl;
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }


  // Test with local-support transform. Should FAIL.
  // Such transforms are not yet supported.
  metric->SetHasLocalSupport(true);
  ITK_TRY_EXPECT_EXCEPTION(itkOptimizer->StartOptimization());

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
    std::cout << "STREAMED ENUM VALUE LBFGS2Optimizerv4Enums::LineSearchMethod: " << ee << std::endl;
  }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
