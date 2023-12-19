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

#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkNormalVariateGenerator.h"
#include "itkCommand.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

namespace itk
{

/**
 * \class OnePlusOneCostFunction
 *
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
 */
class OnePlusOneCostFunction : public itk::SingleValuedCostFunction
{
public:
  using Self = OnePlusOneCostFunction;
  using Superclass = itk::SingleValuedCostFunction;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(OnePlusOneCostFunction);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using MeasureType = Superclass::MeasureType;

  OnePlusOneCostFunction() = default;


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
  GetDerivative(const ParametersType & itkNotUsed(parameters), DerivativeType & itkNotUsed(derivative)) const override
  {
    itkGenericExceptionMacro("OnePlusOneEvolutionaryOptimizer is not supposed to call GetDerivative()");
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

private:
};

class OnePlusOneCommandIterationUpdate : public itk::Command
{
public:
  using Self = OnePlusOneCommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  OnePlusOneCommandIterationUpdate() { m_LastMetricValue = 0.0; };

public:
  using OptimizerType = itk::OnePlusOneEvolutionaryOptimizer;
  using OptimizerPointer = const OptimizerType *;

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    auto optimizer = static_cast<OptimizerPointer>(object);
    if (!itk::IterationEvent().CheckEvent(&event))
    {
      return;
    }
    double currentValue = optimizer->GetValue();
    // Only print out when the Metric value changes
    if (itk::Math::abs(m_LastMetricValue - currentValue) > 1e-7)
    {
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << currentValue << "   ";
      std::cout << optimizer->GetCurrentPosition() << std::endl;
      m_LastMetricValue = currentValue;
    }
  }

private:
  double m_LastMetricValue;
};

} // namespace itk


int
itkOnePlusOneEvolutionaryOptimizerTest(int, char *[])
{
  using OptimizerType = itk::OnePlusOneEvolutionaryOptimizer;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, OnePlusOneEvolutionaryOptimizer, SingleValuedNonLinearOptimizer);


  ITK_TEST_EXPECT_TRUE(!itkOptimizer->GetInitialized());

  itk::OnePlusOneCommandIterationUpdate::Pointer observer = itk::OnePlusOneCommandIterationUpdate::New();
  itkOptimizer->AddObserver(itk::IterationEvent(), observer);

  // Declaration of the CostFunction
  itk::OnePlusOneCostFunction::Pointer costFunction = itk::OnePlusOneCostFunction::New();


  itkOptimizer->SetCostFunction(costFunction);


  using ParametersType = itk::OnePlusOneCostFunction::ParametersType;

  const unsigned int spaceDimension = costFunction->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType initialPosition(spaceDimension);

  initialPosition[0] = 100;
  initialPosition[1] = -100;

  auto maximize = false;
  itkOptimizer->MinimizeOn();
  ITK_TEST_SET_GET_VALUE(!maximize, itkOptimizer->GetMinimize());
  ITK_TEST_SET_GET_BOOLEAN(itkOptimizer, Maximize, maximize);

  unsigned int maximumIteration = 8000;
  itkOptimizer->SetMaximumIteration(8000);
  ITK_TEST_SET_GET_VALUE(maximumIteration, itkOptimizer->GetMaximumIteration());

  auto growthFactor = 1.05;
  itkOptimizer->SetGrowthFactor(growthFactor);
  ITK_TEST_SET_GET_VALUE(growthFactor, itkOptimizer->GetGrowthFactor());

  auto shrinkFactor = std::pow(growthFactor, -0.25);
  itkOptimizer->SetShrinkFactor(shrinkFactor);
  ITK_TEST_SET_GET_VALUE(shrinkFactor, itkOptimizer->GetShrinkFactor());

  auto initialRadius = 1.01;
  itkOptimizer->SetInitialRadius(initialRadius);
  ITK_TEST_SET_GET_VALUE(initialRadius, itkOptimizer->GetInitialRadius());

  auto epsilon = 0.1;
  itkOptimizer->SetEpsilon(epsilon);
  ITK_TEST_SET_GET_VALUE(epsilon, itkOptimizer->GetEpsilon());

  auto catchGetValueException = false;
  itkOptimizer->SetCatchGetValueException(catchGetValueException);
  ITK_TEST_SET_GET_VALUE(catchGetValueException, itkOptimizer->GetCatchGetValueException());

  auto metricWorstPossibleValue = 0;
  itkOptimizer->SetMetricWorstPossibleValue(metricWorstPossibleValue);
  ITK_TEST_SET_GET_VALUE(metricWorstPossibleValue, itkOptimizer->GetMetricWorstPossibleValue());

  initialRadius = 10;
  itkOptimizer->Initialize(initialRadius);
  ITK_TEST_SET_GET_VALUE(initialRadius, itkOptimizer->GetInitialRadius());
  ITK_TEST_SET_GET_VALUE(growthFactor, itkOptimizer->GetGrowthFactor());
  ITK_TEST_SET_GET_VALUE(shrinkFactor, itkOptimizer->GetShrinkFactor());

  ITK_TEST_EXPECT_TRUE(itkOptimizer->GetInitialized());

  using GeneratorType = itk::Statistics::NormalVariateGenerator;
  auto generator = GeneratorType::New();
  itkOptimizer->SetNormalVariateGenerator(generator);

  itkOptimizer->SetInitialPosition(initialPosition);

  ITK_TRY_EXPECT_NO_EXCEPTION(itkOptimizer->StartOptimization());


  ParametersType finalPosition = itkOptimizer->GetCurrentPosition();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ',';
  std::cout << finalPosition[1] << ')' << std::endl;

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
  std::cout << "Maximize: " << itkOptimizer->GetMaximize() << std::endl;
  std::cout << "Epsilon: " << itkOptimizer->GetEpsilon() << std::endl;
  std::cout << "NumberOfIterations: " << itkOptimizer->GetMaximumIteration() << std::endl;

  itkOptimizer->Print(std::cout);
  std::cout << "Stop description   = " << itkOptimizer->GetStopConditionDescription() << std::endl;

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
