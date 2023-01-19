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

#include "itkPowellOptimizer.h"
#include "itkTestingMacros.h"

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
 * \class PowellBoundedCostFunction
 *
 */
class PowellBoundedCostFunction : public itk::SingleValuedCostFunction
{
public:
  using Self = PowellBoundedCostFunction;
  using Superclass = itk::SingleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(PowellBoundedCostFunction, SingleValuedCostFunction);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  PowellBoundedCostFunction() = default;


  void
  GetDerivative(const ParametersType &, DerivativeType &) const override
  {}

  MeasureType
  GetValue(const ParametersType & parameters) const override
  {
    ++POWELL_CALLS_TO_GET_VALUE;

    double x = parameters[0];
    double y = parameters[1];

    std::cout << "      GetValue( ";
    std::cout << x << " ";
    std::cout << y << ") = ";

    MeasureType measure = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

    std::cout << measure << std::endl;

    return measure;
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

private:
};

int
itkPowellOptimizerTest(int argc, char * argv[])
{
  if (argc != 9)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " maximize stepLength stepTolerance valueTolerance maximumIteration maximumLineIteration "
                 "catchGetValueException metricWorstPossibleValue"
              << std::endl;
    return EXIT_FAILURE;
  }

  using OptimizerType = itk::PowellOptimizer;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, PowellOptimizer, SingleValuedNonLinearOptimizer);


  // Declaration of the CostFunction
  auto costFunction = PowellBoundedCostFunction::New();


  itkOptimizer->SetCostFunction(costFunction);


  using ParametersType = PowellBoundedCostFunction::ParametersType;

  const unsigned int spaceDimension = costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType initialPosition(spaceDimension);

  initialPosition[0] = 100;
  initialPosition[1] = -100;

  auto maximize = static_cast<bool>(std::stoi(argv[1]));
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Maximize, maximize);

  auto stepLength = std::stod(argv[2]);
  itkOptimizer->SetStepLength(stepLength);
  ITK_TEST_SET_GET_VALUE(stepLength, itkOptimizer->GetStepLength());

  auto stepTolerance = std::stod(argv[3]);
  itkOptimizer->SetStepTolerance(stepTolerance);
  ITK_TEST_SET_GET_VALUE(stepTolerance, itkOptimizer->GetStepTolerance());

  auto valueTolerance = std::stod(argv[4]);
  itkOptimizer->SetValueTolerance(valueTolerance);
  ITK_TEST_SET_GET_VALUE(valueTolerance, itkOptimizer->GetValueTolerance());

  auto maximumIteration = static_cast<unsigned int>(std::stoi(argv[5]));
  itkOptimizer->SetMaximumIteration(maximumIteration);
  ITK_TEST_SET_GET_VALUE(maximumIteration, itkOptimizer->GetMaximumIteration());

  auto maximumLineIteration = static_cast<unsigned int>(std::stoi(argv[6]));
  itkOptimizer->SetMaximumLineIteration(maximumLineIteration);
  ITK_TEST_SET_GET_VALUE(maximumLineIteration, itkOptimizer->GetMaximumLineIteration());

  auto catchGetValueException = static_cast<bool>(std::stoi(argv[7]));
  itkOptimizer->SetCatchGetValueException(catchGetValueException);
  ITK_TEST_SET_GET_VALUE(catchGetValueException, itkOptimizer->GetCatchGetValueException());

  auto metricWorstPossibleValue = std::stod(argv[8]);
  itkOptimizer->SetMetricWorstPossibleValue(metricWorstPossibleValue);
  ITK_TEST_SET_GET_VALUE(metricWorstPossibleValue, itkOptimizer->GetMetricWorstPossibleValue());

  itkOptimizer->SetInitialPosition(initialPosition);

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

  // Exercise various member functions.
  std::cout << "CurrentIteration: " << itkOptimizer->GetCurrentIteration() << std::endl;

  ITK_TEST_EXPECT_EQUAL(itkOptimizer->GetValue(), itkOptimizer->GetCurrentCost());

  std::cout << "Value: " << itkOptimizer->GetValue() << std::endl;
  std::cout << "CurrentCost: " << itkOptimizer->GetCurrentCost() << std::endl;
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
