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
#include "itkRegularStepGradientDescentOptimizer.h"
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
 * \class RSGCostFunction
 */
class RSGCostFunction : public itk::SingleValuedCostFunction
{
public:
  using Self = RSGCostFunction;
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


  RSGCostFunction() = default;


  MeasureType
  GetValue(const ParametersType & parameters) const override
  {

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "GetValue( ";
    std::cout << x << ' ';
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
    std::cout << x << ' ';
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
itkRegularStepGradientDescentOptimizerTest(int, char *[])
{
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;

  using ScalesType = OptimizerType::ScalesType;


  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    itkOptimizer, RegularStepGradientDescentOptimizer, RegularStepGradientDescentBaseOptimizer);


  // Declaration of the CostFunction
  auto costFunction = RSGCostFunction::New();


  itkOptimizer->SetCostFunction(costFunction);


  using ParametersType = RSGCostFunction::ParametersType;


  const unsigned int spaceDimension = costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType initialPosition(spaceDimension);
  initialPosition[0] = 100;
  initialPosition[1] = -100;

  ScalesType parametersScale(spaceDimension);
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  auto minimize = true;
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Minimize, minimize);
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Maximize, !minimize);

  itkOptimizer->SetScales(parametersScale);
  ITK_TEST_SET_GET_VALUE(parametersScale, itkOptimizer->GetScales());

  auto gradientMagnitudeTolerance = 1e-6;
  itkOptimizer->SetGradientMagnitudeTolerance(gradientMagnitudeTolerance);
  ITK_TEST_SET_GET_VALUE(gradientMagnitudeTolerance, itkOptimizer->GetGradientMagnitudeTolerance());

  auto maximumStepLength = 30.0;
  itkOptimizer->SetMaximumStepLength(maximumStepLength);
  ITK_TEST_SET_GET_VALUE(maximumStepLength, itkOptimizer->GetMaximumStepLength());

  auto minimumStepLength = 1e-6;
  itkOptimizer->SetMinimumStepLength(minimumStepLength);
  ITK_TEST_SET_GET_VALUE(minimumStepLength, itkOptimizer->GetMinimumStepLength());

  itk::SizeValueType numberOfIterations = static_cast<itk::SizeValueType>(900);
  itkOptimizer->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, itkOptimizer->GetNumberOfIterations());

  itkOptimizer->SetInitialPosition(initialPosition);
  ITK_TEST_SET_GET_VALUE(initialPosition, itkOptimizer->GetInitialPosition());

  ITK_TRY_EXPECT_NO_EXCEPTION(itkOptimizer->StartOptimization());


  ParametersType finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ',';
  std::cout << finalPosition[1] << ')' << std::endl;

  // Check results to see if it is within range
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


  // Run now with a different relaxation factor
  {
    itkOptimizer->SetInitialPosition(initialPosition);

    auto relaxationFactor = 0.8;
    itkOptimizer->SetRelaxationFactor(relaxationFactor);
    ITK_TEST_SET_GET_VALUE(relaxationFactor, itkOptimizer->GetRelaxationFactor());

    ITK_TRY_EXPECT_NO_EXCEPTION(itkOptimizer->StartOptimization());


    finalPosition = itkOptimizer->GetCurrentPosition();
    std::cout << "Solution        = (";
    std::cout << finalPosition[0] << ',';
    std::cout << finalPosition[1] << ')' << std::endl;

    // Check results to see if it is within range
    pass = true;
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
  }

  // Verify that the optimizer doesn't run if the
  // number of iterations is set to zero.
  {
    itkOptimizer->SetNumberOfIterations(0);
    itkOptimizer->SetInitialPosition(initialPosition);

    ITK_TRY_EXPECT_NO_EXCEPTION(itkOptimizer->StartOptimization());


    if (itkOptimizer->GetCurrentIteration() > 0)
    {
      std::cerr << "The optimizer is running iterations despite of ";
      std::cerr << "having a maximum number of iterations set to zero" << std::endl;
      return EXIT_FAILURE;
    }
  }

  //
  // Test the Exception if the GradientMagnitudeTolerance is set to a negative value
  //
  itkOptimizer->SetGradientMagnitudeTolerance(-1.0);

  ITK_TRY_EXPECT_EXCEPTION(itkOptimizer->StartOptimization());


  // Test streaming enumeration for
  // RegularStepGradientDescentBaseOptimizerEnums::StopCondition elements
  const std::set<itk::RegularStepGradientDescentBaseOptimizerEnums::StopCondition> allStopCondition{
    itk::RegularStepGradientDescentBaseOptimizerEnums::StopCondition::GradientMagnitudeTolerance,
    itk::RegularStepGradientDescentBaseOptimizerEnums::StopCondition::StepTooSmall,
    itk::RegularStepGradientDescentBaseOptimizerEnums::StopCondition::ImageNotAvailable,
    itk::RegularStepGradientDescentBaseOptimizerEnums::StopCondition::CostFunctionError,
    itk::RegularStepGradientDescentBaseOptimizerEnums::StopCondition::MaximumNumberOfIterations,
    itk::RegularStepGradientDescentBaseOptimizerEnums::StopCondition::Unknown
  };
  for (const auto & ee : allStopCondition)
  {
    std::cout << "STREAMED ENUM VALUE "
                 "RegularStepGradientDescentBaseOptimizerEnums::StopCondition: "
              << ee << std::endl;
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
