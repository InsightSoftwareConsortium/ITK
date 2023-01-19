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
#include "itkSPSAOptimizer.h"
#include "itkTestingMacros.h"


/**
 * \class
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
 */
class SPSACostFunction : public itk::SingleValuedCostFunction
{
public:
  using Self = SPSACostFunction;
  using Superclass = itk::SingleValuedCostFunction;
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


  SPSACostFunction() = default;


  MeasureType
  GetValue(const ParametersType & parameters) const override
  {

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetValue( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    MeasureType measure = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

    std::cout << measure << std::endl;
    return measure;
  }

  void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const override
  {

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetDerivative( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    derivative = DerivativeType(SpaceDimension);
    derivative[0] = 3 * x + 2 * y - 2;
    derivative[1] = 2 * x + 6 * y + 8;
  }


  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

private:
};

int
itkSPSAOptimizerTest(int, char *[])
{
  std::cout << "SPSAOptimizer Test ";
  std::cout << std::endl << std::endl;

  using OptimizerType = itk::SPSAOptimizer;
  using ScalesType = OptimizerType::ScalesType;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, SPSAOptimizer, SingleValuedNonLinearOptimizer);


  // Declaration of the CostFunction
  auto costFunction = SPSACostFunction::New();
  itkOptimizer->SetCostFunction(costFunction);

  using ParametersType = SPSACostFunction::ParametersType;
  const unsigned int spaceDimension = costFunction->GetNumberOfParameters();

  ScalesType parametersScale(spaceDimension);
  parametersScale[0] = 1.0;
  parametersScale[1] = 2.0;
  itkOptimizer->SetScales(parametersScale);

  bool maximize = false;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Maximize, maximize);

  bool minimize = !maximize;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Minimize, minimize);

  double a = 10.0;
  itkOptimizer->SetA(a);
  ITK_TEST_SET_GET_VALUE(a, itkOptimizer->GetA());

  double alpha = 0.602;
  itkOptimizer->SetAlpha(alpha);
  ITK_TEST_SET_GET_VALUE(alpha, itkOptimizer->GetAlpha());

  double c = 0.0001;
  itkOptimizer->Setc(c);
  ITK_TEST_SET_GET_VALUE(c, itkOptimizer->Getc());

  itkOptimizer->SetSc(c);
  ITK_TEST_SET_GET_VALUE(c, itkOptimizer->GetSc());

  double gamma = 0.101;
  itkOptimizer->SetGamma(gamma);
  ITK_TEST_SET_GET_VALUE(gamma, itkOptimizer->GetGamma());

  double tolerance = 1e-5;
  itkOptimizer->SetTolerance(tolerance);
  ITK_TEST_SET_GET_VALUE(tolerance, itkOptimizer->GetTolerance());

  double stateOfConvergenceDecayRate = 0.5;
  itkOptimizer->SetStateOfConvergenceDecayRate(stateOfConvergenceDecayRate);
  ITK_TEST_SET_GET_VALUE(stateOfConvergenceDecayRate, itkOptimizer->GetStateOfConvergenceDecayRate());

  itk::SizeValueType minimumNumberOfIterations = 10;
  itkOptimizer->SetMinimumNumberOfIterations(10);
  ITK_TEST_SET_GET_VALUE(minimumNumberOfIterations, itkOptimizer->GetMinimumNumberOfIterations());

  itk::SizeValueType maximumNumberOfIterations = 100;
  itkOptimizer->SetMaximumNumberOfIterations(maximumNumberOfIterations);
  ITK_TEST_SET_GET_VALUE(maximumNumberOfIterations, itkOptimizer->GetMaximumNumberOfIterations());

  itk::SizeValueType numberOfPerturbations = 1;
  itkOptimizer->SetNumberOfPerturbations(numberOfPerturbations);
  ITK_TEST_SET_GET_VALUE(numberOfPerturbations, itkOptimizer->GetNumberOfPerturbations());

  // We start not so far from  | 2 -2 |
  ParametersType initialPosition(spaceDimension);
  initialPosition[0] = 100;
  initialPosition[1] = -100;
  itkOptimizer->SetInitialPosition(initialPosition);

  try
  {
    itkOptimizer->GuessParameters(50, 70.0);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Guessing Parameters" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "\nEstimated parameter: a = " << itkOptimizer->Geta();
  std::cout << "\nEstimated parameter: A = " << itkOptimizer->GetA() << "\n" << std::endl;

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

  std::cout << "StateOfConvergence in last iteration: " << itkOptimizer->GetStateOfConvergence() << std::endl;
  std::cout << "NumberOfIterations: " << itkOptimizer->GetCurrentIteration() << std::endl;

  std::cout << "Stop condition: " << itkOptimizer->GetStopConditionDescription() << std::endl;

  std::cout << "LearningRate: " << itkOptimizer->GetLearningRate() << std::endl;
  std::cout << "GradientMagnitude: " << itkOptimizer->GetGradientMagnitude() << std::endl;
  std::cout << "Gradient: " << itkOptimizer->GetGradient() << std::endl;

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
  if (itkOptimizer->GetStopCondition() == itk::SPSAOptimizer::StopConditionSPSAOptimizerEnum::Unknown)
  {
    pass = false;
  }
  if (itkOptimizer->GetStopCondition() == itk::SPSAOptimizer::StopConditionSPSAOptimizerEnum::MetricError)
  {
    pass = false;
  }

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  // Test streaming enumeration for SPSAOptimizerEnums::StopConditionSPSAOptimizer elements
  const std::set<itk::SPSAOptimizerEnums::StopConditionSPSAOptimizer> allStopConditionSPSAOptimizer{
    itk::SPSAOptimizerEnums::StopConditionSPSAOptimizer::Unknown,
    itk::SPSAOptimizerEnums::StopConditionSPSAOptimizer::MaximumNumberOfIterations,
    itk::SPSAOptimizerEnums::StopConditionSPSAOptimizer::BelowTolerance,
    itk::SPSAOptimizerEnums::StopConditionSPSAOptimizer::MetricError
  };
  for (const auto & ee : allStopConditionSPSAOptimizer)
  {
    std::cout << "STREAMED ENUM VALUE SPSAOptimizerEnums::StopConditionSPSAOptimizer: " << ee << std::endl;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
