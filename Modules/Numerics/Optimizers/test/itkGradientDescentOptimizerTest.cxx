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
#include "itkGradientDescentOptimizer.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


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
 *
 * \class gradientCostFunction
 */
class gradientCostFunction : public itk::SingleValuedCostFunction
{
public:
  using Self = gradientCostFunction;
  using Superclass = itk::SingleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(gradientCostFunction, SingleValuedCostFunction);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  gradientCostFunction() = default;


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

    DerivativeType temp(SpaceDimension);
    temp.Fill(0);
    derivative = temp;
    derivative[0] = 3 * x + 2 * y - 2;
    derivative[1] = 2 * x + 6 * y + 8;

    std::cout << derivative << std::endl;
  }


  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

private:
};

int
itkGradientDescentOptimizerTest(int, char *[])
{
  std::cout << "Gradient Descent Optimizer Test ";
  std::cout << std::endl << std::endl;

  using OptimizerType = itk::GradientDescentOptimizer;


  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, GradientDescentOptimizer, SingleValuedNonLinearOptimizer);


  // Declaration of the CostFunction
  auto costFunction = gradientCostFunction::New();


  itkOptimizer->SetCostFunction(costFunction);


  using ParametersType = gradientCostFunction::ParametersType;

  const unsigned int spaceDimension = costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType initialPosition(spaceDimension);

  initialPosition[0] = 100;
  initialPosition[1] = -100;


  bool maximize = false;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Maximize, maximize);

  bool minimize = !maximize;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Minimize, minimize);

  double learningRate = 0.1;
  itkOptimizer->SetLearningRate(learningRate);
  ITK_TEST_SET_GET_VALUE(learningRate, itkOptimizer->GetLearningRate());

  itk::SizeValueType numberOfIterations = 50;
  itkOptimizer->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, itkOptimizer->GetNumberOfIterations());

  itkOptimizer->SetInitialPosition(initialPosition);
  ITK_TEST_SET_GET_VALUE(initialPosition, itkOptimizer->GetInitialPosition());

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

  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;
  std::cout << "StopCondition: " << itkOptimizer->GetStopCondition() << std::endl;
  std::cout << "Value: " << itkOptimizer->GetValue() << std::endl;
  std::cout << "Gradient: " << itkOptimizer->GetGradient() << std::endl;

  // Test streaming enumeration for GradientDescentOptimizerEnums::StopConditionGradientDescentOptimizer elements
  const std::set<itk::GradientDescentOptimizerEnums::StopConditionGradientDescentOptimizer>
    allStopConditionGradientDescentOptimizer{
      itk::GradientDescentOptimizerEnums::StopConditionGradientDescentOptimizer::MaximumNumberOfIterations,
      itk::GradientDescentOptimizerEnums::StopConditionGradientDescentOptimizer::MetricError
    };
  for (const auto & ee : allStopConditionGradientDescentOptimizer)
  {
    std::cout << "STREAMED ENUM VALUE GradientDescentOptimizerEnums::StopConditionGradientDescentOptimizer: " << ee
              << std::endl;
  }

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
