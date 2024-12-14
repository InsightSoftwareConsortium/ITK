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

#include <algorithm>

#include "itkCommand.h"
#include "itkExhaustiveOptimizerv4.h"

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
 * \class ExhaustiveOptv4Metric
 */
class ExhaustiveOptv4Metric : public itk::ObjectToObjectMetricBase
{
public:
  using Self = ExhaustiveOptv4Metric;
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


  ExhaustiveOptv4Metric() { m_HasLocalSupport = false; }

  MeasureType
  GetValue() const override
  {
    const double x = this->m_Parameters[0];
    const double y = this->m_Parameters[1];

    std::cout << "GetValue ( " << x << " , " << y << ") = ";

    const MeasureType val = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

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


class IndexObserver : public itk::Command
{
public:
  using Self = IndexObserver;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;

  itkNewMacro(IndexObserver);

  void
  Execute(const itk::Object * caller, const itk::EventObject &) override
  {
    using OptimizerType = itk::ExhaustiveOptimizerv4<double>;
    const auto * optimizer = dynamic_cast<const OptimizerType *>(caller);

    if (nullptr != optimizer)
    {
      OptimizerType::ParametersType currentIndex = optimizer->GetCurrentIndex();
      const itk::SizeValueType      currentIteration = optimizer->GetCurrentIteration();

      if (currentIndex.GetSize() == 2)
      {
        std::cout << currentIteration << ": ";
        std::cout << " @ index = " << currentIndex << '\n';
        // Casting is safe here since the indices are always integer values (but there are stored in doubles):
        auto idx = static_cast<unsigned long>(currentIndex[0] + 21 * currentIndex[1]);
        m_VisitedIndices.push_back(idx);
      }
    }
  }

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute(static_cast<const itk::Object *>(caller), event);
  }

  std::vector<unsigned long> m_VisitedIndices;
};

int
itkExhaustiveOptimizerv4Test(int, char *[])
{
  std::cout << "ExhaustiveOptimizerv4 Test ";
  std::cout << '\n' << '\n';

  using OptimizerType = itk::ExhaustiveOptimizerv4<double>;

  using ScalesType = OptimizerType::ScalesType;


  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, ExhaustiveOptimizerv4, ObjectToObjectOptimizerBaseTemplate);


  // Index observer (enables us to check if all positions were indeed visisted):
  auto idxObserver = IndexObserver::New();
  itkOptimizer->AddObserver(itk::IterationEvent(), idxObserver);

  // Declaration of the CostFunction
  auto metric = ExhaustiveOptv4Metric::New();
  itkOptimizer->SetMetric(metric);


  using ParametersType = ExhaustiveOptv4Metric::ParametersType;


  const unsigned int spaceDimension = metric->GetNumberOfParameters();

  // We start not so far from  | 2 -2 |
  ParametersType initialPosition(spaceDimension);
  initialPosition[0] = 0.0;
  initialPosition[1] = -4.0;

  // Set the initial position by setting the metric
  // parameters.
  std::cout << "Set metric parameters." << '\n';
  metric->SetParameters(initialPosition);


  ScalesType parametersScale(spaceDimension);
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  itkOptimizer->SetScales(parametersScale);


  auto stepLength = 1.0;
  itkOptimizer->SetStepLength(stepLength);
  ITK_TEST_SET_GET_VALUE(stepLength, itkOptimizer->GetStepLength());


  using StepsType = OptimizerType::StepsType;
  StepsType steps(spaceDimension);
  steps[0] = 10;
  steps[1] = 10;

  itkOptimizer->SetNumberOfSteps(steps);
  ITK_TEST_SET_GET_VALUE(steps, itkOptimizer->GetNumberOfSteps());


  try
  {
    itkOptimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << '\n';
    std::cout << "An error occurred during Optimization" << '\n';
    std::cout << "Location    = " << e.GetLocation() << '\n';
    std::cout << "Description = " << e.GetDescription() << '\n';
    return EXIT_FAILURE;
  }


  const bool minimumValuePass = itk::Math::abs(itkOptimizer->GetMinimumMetricValue() - -10) < 1E-3;

  std::cout << "MinimumMetricValue = " << itkOptimizer->GetMinimumMetricValue() << '\n';
  std::cout << "Minimum Position = " << itkOptimizer->GetMinimumMetricValuePosition() << '\n';

  const bool maximumValuePass = itk::Math::abs(itkOptimizer->GetMaximumMetricValue() - 926) < 1E-3;
  std::cout << "MaximumMetricValue = " << itkOptimizer->GetMaximumMetricValue() << '\n';
  std::cout << "Maximum Position = " << itkOptimizer->GetMaximumMetricValuePosition() << '\n';

  ParametersType finalPosition = itkOptimizer->GetMinimumMetricValuePosition();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ',';
  std::cout << finalPosition[1] << ')' << '\n';

  std::cout << "CurrentValue: " << itkOptimizer->GetCurrentValue() << '\n';

  bool                       visitedIndicesPass = true;
  std::vector<unsigned long> visitedIndices = idxObserver->m_VisitedIndices;

  const size_t requiredNumberOfSteps = (2 * steps[0] + 1) * (2 * steps[1] + 1);
  if (visitedIndices.size() != requiredNumberOfSteps)
  {
    visitedIndicesPass = false;
  }

  std::sort(visitedIndices.begin(), visitedIndices.end());

  for (size_t i = 0; i < visitedIndices.size(); ++i)
  {
    if (visitedIndices[i] != i)
    {
      visitedIndicesPass = false;
      std::cout << "Mismatch in visited index " << visitedIndices[i] << " @ " << i << '\n';
      break;
    }
  }

  //
  // check results to see if it is within range
  //
  bool         trueParamsPass = true;
  const double trueParameters[2] = { 2, -2 };
  for (unsigned int j = 0; j < 2; ++j)
  {
    if (itk::Math::abs(finalPosition[j] - trueParameters[j]) > 0.01)
    {
      trueParamsPass = false;
    }
  }

  if (!minimumValuePass || !maximumValuePass || !trueParamsPass || !visitedIndicesPass)
  {
    std::cout << "minimumValuePass   = " << minimumValuePass << '\n';
    std::cout << "maximumValuePass   = " << maximumValuePass << '\n';
    std::cout << "trueParamsPass     = " << trueParamsPass << '\n';
    std::cout << "visitedIndicesPass = " << visitedIndicesPass << '\n';
    std::cout << "Test failed." << '\n';
    return EXIT_FAILURE;
  }


  std::cout << "Test passed." << '\n';
  return EXIT_SUCCESS;
}
