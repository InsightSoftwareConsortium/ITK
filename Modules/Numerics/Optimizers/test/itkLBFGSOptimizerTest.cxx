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

#include "itkLBFGSOptimizer.h"
#include "itkMath.h"
#include "itkTestingMacros.h"
#include <iostream>

/**
 * \class A cost function
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
class LBFGSCostFunction : public itk::SingleValuedCostFunction
{
public:
  using Self = LBFGSCostFunction;
  using Superclass = itk::SingleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(LBFCostFunction, SingleValuedCostFunction);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;

  using VectorType = vnl_vector<double>;
  using MatrixType = vnl_matrix<double>;

  using MeasureType = double;

  LBFGSCostFunction() = default;

  double
  GetValue(const ParametersType & position) const override
  {
    double x = position[0];
    double y = position[1];

    std::cout << "GetValue ( ";
    std::cout << x << " , " << y;
    std::cout << ") = ";

    double val = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

    std::cout << val << std::endl;

    return val;
  }

  void
  GetDerivative(const ParametersType & position, DerivativeType & derivative) const override
  {
    double x = position[0];
    double y = position[1];

    std::cout << "GetDerivative ( ";
    std::cout << x << " , " << y;
    std::cout << ") = ";

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = 3 * x + 2 * y - 2;
    derivative[1] = 2 * x + 6 * y + 8;
    std::cout << "(";
    std::cout << derivative[0] << " , ";
    std::cout << derivative[1] << ")" << std::endl;
  }


  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

private:
};


int
itkLBFGSOptimizerTest(int, char *[])
{
  std::cout << "LBFGS Optimizer Test \n \n";

  using OptimizerType = itk::LBFGSOptimizer;
  using vnlOptimizerType = OptimizerType::InternalOptimizerType;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, LBFGSOptimizer, SingleValuedNonLinearVnlOptimizer);


  // Declaration of the CostFunction adapter
  auto costFunction = LBFGSCostFunction::New();

  // Set some optimizer parameters
  bool trace = false;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Trace, trace);

  unsigned int maximumNumberOfFunctionEvaluations = 1000;
  itkOptimizer->SetMaximumNumberOfFunctionEvaluations(maximumNumberOfFunctionEvaluations);
  ITK_TEST_SET_GET_VALUE(maximumNumberOfFunctionEvaluations, itkOptimizer->GetMaximumNumberOfFunctionEvaluations());

  double gradientConvergenceTolerance = 1e-3;
  itkOptimizer->SetGradientConvergenceTolerance(gradientConvergenceTolerance);
  ITK_TEST_SET_GET_VALUE(gradientConvergenceTolerance, itkOptimizer->GetGradientConvergenceTolerance());

  double lineSearchAccuracy = 0.1;
  itkOptimizer->SetLineSearchAccuracy(lineSearchAccuracy);
  ITK_TEST_SET_GET_VALUE(lineSearchAccuracy, itkOptimizer->GetLineSearchAccuracy());

  double defaultStepLength = 5.0;
  itkOptimizer->SetDefaultStepLength(defaultStepLength);
  ITK_TEST_SET_GET_VALUE(defaultStepLength, itkOptimizer->GetDefaultStepLength());

  std::cout << "GetValue() before optimizer starts: " << itkOptimizer->GetValue() << std::endl;
  itkOptimizer->SetCostFunction(costFunction);

  // const double F_Tolerance      = 1e-3;  // Function value tolerance: not used
  // const double X_Tolerance      = 1e-8;  // Search space tolerance: not used
  // const double Epsilon_Function = 1e-10; // Step : not used

  vnlOptimizerType * vnlOptimizer = itkOptimizer->GetOptimizer();

  vnlOptimizer->set_check_derivatives(0);

  constexpr unsigned int        SpaceDimension = 2;
  OptimizerType::ParametersType initialValue(SpaceDimension);

  // We start not so far from  | 2 -2 |
  initialValue[0] = 100;
  initialValue[1] = -100;

  OptimizerType::ParametersType currentValue(2);

  currentValue = initialValue;

  itkOptimizer->SetInitialPosition(currentValue);

  // Set some optimizer parameters
  maximumNumberOfFunctionEvaluations = 100;
  itkOptimizer->SetMaximumNumberOfFunctionEvaluations(maximumNumberOfFunctionEvaluations);
  ITK_TEST_SET_GET_VALUE(maximumNumberOfFunctionEvaluations, itkOptimizer->GetMaximumNumberOfFunctionEvaluations());

  gradientConvergenceTolerance = 1e-4;
  itkOptimizer->SetGradientConvergenceTolerance(gradientConvergenceTolerance);
  ITK_TEST_SET_GET_VALUE(gradientConvergenceTolerance, itkOptimizer->GetGradientConvergenceTolerance());

  lineSearchAccuracy = 0.9;
  itkOptimizer->SetLineSearchAccuracy(lineSearchAccuracy);
  ITK_TEST_SET_GET_VALUE(lineSearchAccuracy, itkOptimizer->GetLineSearchAccuracy());

  defaultStepLength = 1.0;
  itkOptimizer->SetDefaultStepLength(defaultStepLength);
  ITK_TEST_SET_GET_VALUE(defaultStepLength, itkOptimizer->GetDefaultStepLength());

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

  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

  std::cout << "End condition   = " << vnlOptimizer->get_failure_code() << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer->get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer->get_num_evaluations() << std::endl;
  std::cout << std::endl;

  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

  std::cout << "Solution        = (" << finalPosition[0] << "," << finalPosition[1] << ")" << std::endl;

  std::cout << "End condition   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

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

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  // Get the final value of the optimizer
  std::cout << "Testing GetValue() : ";
  OptimizerType::MeasureType finalValue = itkOptimizer->GetValue();
  if (itk::Math::abs(finalValue + 10.0) > 0.01)
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[SUCCESS]" << std::endl;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
