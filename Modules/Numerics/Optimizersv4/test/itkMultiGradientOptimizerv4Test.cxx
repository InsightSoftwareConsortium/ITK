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
#include "itkMultiGradientOptimizerv4.h"
#include "itkTestingMacros.h"

/**
 *  \class MultiGradientOptimizerv4TestMetric
 *
 *  The objective function is the summation of two quadratics of form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is a matrix and b is a vector
 *
 *  The systems in this example are:
 *
 *  Metric1
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *  Metric2
 *
 *     | 3  2 ||x|   | 1|   |0|
 *     | 2  6 ||y| + |-4| = |0|
 *
 *   the weighted optimal solution is the vector | 1.5 -1.5 |
 *
 */
class MultiGradientOptimizerv4TestMetric : public itk::ObjectToObjectMetricBase
{
public:
  using Self = MultiGradientOptimizerv4TestMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(MultiGradientOptimizerv4TestMetric, ObjectToObjectMetricBase);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using ParametersPointer = Superclass::ParametersType *;
  using ParametersValueType = Superclass::ParametersValueType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  MultiGradientOptimizerv4TestMetric() = default;

  void
  Initialize() override
  {}

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    derivative.Fill(itk::NumericTraits<ParametersValueType>::ZeroValue());
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    if (derivative.Size() != 2)
    {
      derivative.SetSize(2);
    }

    double x = (*m_Parameters)[0];
    double y = (*m_Parameters)[1];

    std::cout << "GetValueAndDerivative( ";
    std::cout << x << " ";
    std::cout << y << ") = " << std::endl;

    value = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;

    std::cout << "value: " << value << std::endl;

    /* The optimizer simply takes the derivative from the metric
     * and adds it to the transform after scaling. So instead of
     * setting a 'minimize' option in the gradient, we return
     * a minimizing derivative. */
    derivative[0] = -(3 * x + 2 * y - 2);
    derivative[1] = -(2 * x + 6 * y + 8);

    std::cout << "derivative: " << derivative << std::endl;
  }

  MeasureType
  GetValue() const override
  {
    double x = (*m_Parameters)[0];
    double y = (*m_Parameters)[1];
    double metric = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;
    std::cout << (*m_Parameters) << " metric " << metric << std::endl;
    return metric;
  }

  void
  UpdateTransformParameters(const DerivativeType & update, ParametersValueType) override
  {
    (*m_Parameters) += update;
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

  bool
  HasLocalSupport() const override
  {
    return false;
  }

  unsigned int
  GetNumberOfLocalParameters() const override
  {
    return SpaceDimension;
  }

  /* These Set/Get methods are only needed for this test derivation that
   * isn't using a transform */
  void
  SetParameters(ParametersType & parameters) override
  {
    m_Parameters = &parameters;
  }

  const ParametersType &
  GetParameters() const override
  {
    return (*m_Parameters);
  }

private:
  ParametersPointer m_Parameters{ nullptr };
};

/** A second test metric with slightly different optimum */
class MultiGradientOptimizerv4TestMetric2 : public itk::ObjectToObjectMetricBase
{
public:
  using Self = MultiGradientOptimizerv4TestMetric2;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(MultiGradientOptimizerv4TestMetric2, ObjectToObjectMetricBase);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using ParametersPointer = Superclass::ParametersType *;
  using ParametersValueType = Superclass::ParametersValueType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  MultiGradientOptimizerv4TestMetric2() = default;

  void
  Initialize() override
  {}

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    derivative.Fill(itk::NumericTraits<ParametersValueType>::ZeroValue());
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    if (derivative.Size() != 2)
    {
      derivative.SetSize(2);
    }

    double x = (*m_Parameters)[0];
    double y = (*m_Parameters)[1];

    std::cout << "GetValueAndDerivative( ";
    std::cout << x << " ";
    std::cout << y << ") = " << std::endl;

    value = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - x + 4 * y;

    std::cout << "value: " << value << std::endl;

    /* The optimizer simply takes the derivative from the metric
     * and adds it to the transform after scaling. So instead of
     * setting a 'minimize' option in the gradient, we return
     * a minimizing derivative. */
    derivative[0] = -(3 * x + 2 * y - 1);
    derivative[1] = -(2 * x + 6 * y + 4);

    std::cout << "derivative: " << derivative << std::endl;
  }

  MeasureType
  GetValue() const override
  {
    double x = (*m_Parameters)[0];
    double y = (*m_Parameters)[1];
    double metric = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - x + 4 * y;
    std::cout << (*m_Parameters) << " metric " << metric << std::endl;
    return metric;
  }

  bool
  HasLocalSupport() const override
  {
    return false;
  }

  void
  UpdateTransformParameters(const DerivativeType & update, ParametersValueType) override
  {
    (*m_Parameters) += update;
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return SpaceDimension;
  }

  unsigned int
  GetNumberOfLocalParameters() const override
  {
    return SpaceDimension;
  }

  /* These Set/Get methods are only needed for this test derivation that
   * isn't using a transform */
  void
  SetParameters(ParametersType & parameters) override
  {
    m_Parameters = &parameters;
  }

  const ParametersType &
  GetParameters() const override
  {
    return (*m_Parameters);
  }

private:
  ParametersPointer m_Parameters{ nullptr };
};

///////////////////////////////////////////////////////////
/** This metric has an optimum at (1,-1) and when we
 *  combine its gradient with that of the metric above
 *  we expect an average result with solution @ (1.5,-1.5)
 */
///////////////////////////////////////////////////////////
int
MultiGradientOptimizerv4RunTest(itk::MultiGradientOptimizerv4::Pointer & itkOptimizer)
{
  try
  {
    std::cout << "currentPosition before optimization: " << itkOptimizer->GetCurrentPosition() << std::endl;
    itkOptimizer->StartOptimization();
    std::cout << "currentPosition after optimization: " << itkOptimizer->GetCurrentPosition() << std::endl;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "StopCondition: " << itkOptimizer->GetStopCondition() << std::endl;

  using ParametersType = MultiGradientOptimizerv4TestMetric::ParametersType;
  ParametersType finalPosition = itkOptimizer->GetMetric()->GetParameters();

  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;

  //
  // check results to see if it is within range
  //
  ParametersType trueParameters(2);
  trueParameters[0] = 1.5;
  trueParameters[1] = -1.5;
  for (itk::SizeValueType j = 0; j < 2; ++j)
  {
    if (itk::Math::abs(finalPosition[j] - trueParameters[j]) > 0.01)
    {
      std::cerr << "Results do not match: " << std::endl
                << "expected: " << trueParameters << std::endl
                << "returned: " << finalPosition << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
///////////////////////////////////////////////////////////
int
itkMultiGradientOptimizerv4Test(int, char *[])
{
  std::cout << "MultiGradient descent Optimizer Test ";
  std::cout << std::endl << std::endl;

  using OptimizerType = itk::MultiGradientOptimizerv4;
  using ParametersType = MultiGradientOptimizerv4TestMetric::ParametersType;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, MultiGradientOptimizerv4Template, GradientDescentOptimizerv4Template);


  // Declaration of the Metric
  auto                   metric = MultiGradientOptimizerv4TestMetric::New();
  auto                   metric2 = MultiGradientOptimizerv4TestMetric2::New();
  constexpr unsigned int spaceDimension = 2;
  itkOptimizer->SetMetric(metric);
  itkOptimizer->SetNumberOfIterations(50);

  OptimizerType::OptimizersListType optimizersList = itkOptimizer->GetOptimizersList();

  /** Declare the first optimizer for metric 1 */
  OptimizerType::LocalOptimizerPointer locoptimizer = OptimizerType::LocalOptimizerType::New();
  locoptimizer->SetLearningRate(1.e-1);
  locoptimizer->SetNumberOfIterations(25);
  locoptimizer->SetMetric(metric);
  locoptimizer->SetNumberOfIterations(1);
  optimizersList.push_back(locoptimizer);

  /** Declare the 2nd optimizer for metric 2 */
  OptimizerType::LocalOptimizerPointer locoptimizer2 = OptimizerType::LocalOptimizerType::New();
  locoptimizer2->SetLearningRate(1.e-1);
  locoptimizer2->SetNumberOfIterations(25);
  locoptimizer2->SetMetric(metric2);
  locoptimizer->SetNumberOfIterations(1);
  optimizersList.push_back(locoptimizer2);

  /** Pass the list back to the combined optimizer */
  itkOptimizer->SetOptimizersList(optimizersList);

  /*
   * Test 1
   */
  // We start not so far from  | 1.5 -1.5 |
  ParametersType testPosition(spaceDimension);
  testPosition[0] = 7.5;
  testPosition[1] = 9.5;
  /** Note: both metrics have the same transforms and parameters */
  /** We need the parameters to be the same object across all metric instances*/
  metric->SetParameters(testPosition);
  metric2->SetParameters(testPosition);
  // test the optimization
  std::cout << "Test optimization with equal weights on each metric:" << std::endl;
  if (MultiGradientOptimizerv4RunTest(itkOptimizer) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }
  std::cout << "Test 1 passed." << std::endl;
  return EXIT_SUCCESS;
}
