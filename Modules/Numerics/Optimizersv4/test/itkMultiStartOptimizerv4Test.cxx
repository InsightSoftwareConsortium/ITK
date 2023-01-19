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
#include "itkMultiStartOptimizerv4.h"
#include "itkTestingMacros.h"

/**
 *  \class MultiStartOptimizerv4TestMetric for test
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
class MultiStartOptimizerv4TestMetric : public itk::ObjectToObjectMetricBase
{
public:
  using Self = MultiStartOptimizerv4TestMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(MultiStartOptimizerv4TestMetric, ObjectToObjectMetricBase);

  enum
  {
    SpaceDimension = 2
  };

  using ParametersType = Superclass::ParametersType;
  using ParametersValueType = Superclass::ParametersValueType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  MultiStartOptimizerv4TestMetric()
  {
    m_Parameters.SetSize(SpaceDimension);
    m_Parameters.Fill(0);
  }

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

    double x = m_Parameters[0];
    double y = m_Parameters[1];

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
    double x = m_Parameters[0];
    double y = m_Parameters[1];
    double metric = 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;
    std::cout << m_Parameters << " metric " << metric << std::endl;
    return metric;
  }

  void
  UpdateTransformParameters(const DerivativeType & update, ParametersValueType) override
  {
    m_Parameters += update;
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
    m_Parameters = parameters;
  }

  const ParametersType &
  GetParameters() const override
  {
    return m_Parameters;
  }

private:
  ParametersType m_Parameters;
};

///////////////////////////////////////////////////////////
int
MultiStartOptimizerv4RunTest(itk::MultiStartOptimizerv4::Pointer & itkOptimizer)
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

  using ParametersType = MultiStartOptimizerv4TestMetric::ParametersType;
  ParametersType finalPosition = itkOptimizer->GetMetric()->GetParameters();
  ParametersType bestPosition = itkOptimizer->GetBestParameters();

  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << ",";
  std::cout << finalPosition[1] << ")" << std::endl;
  std::cout << "Best Solution   = (";
  std::cout << bestPosition[0] << ",";
  std::cout << bestPosition[1] << ")" << std::endl;

  //
  // check results to see if it is within range
  //
  ParametersType trueParameters(2);
  trueParameters[0] = 2.0;
  trueParameters[1] = -2.0;
  for (itk::SizeValueType j = 0; j < 2; ++j)
  {
    if (itk::Math::abs(bestPosition[j] - trueParameters[j]) > 0.01)
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
itkMultiStartOptimizerv4Test(int, char *[])
{
  std::cout << "MultiStart  Optimizer Test ";
  std::cout << std::endl << std::endl;

  using OptimizerType = itk::MultiStartOptimizerv4;

  // Declaration of an itkOptimizer
  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(itkOptimizer, MultiStartOptimizerv4Template, ObjectToObjectOptimizerBaseTemplate);


  auto stopCondition = itk::StopConditionObjectToObjectOptimizerEnum::MAXIMUM_NUMBER_OF_ITERATIONS;
  ITK_TEST_EXPECT_EQUAL(stopCondition, itkOptimizer->GetStopCondition());

  // Declaration of the Metric
  auto metric = MultiStartOptimizerv4TestMetric::New();

  itkOptimizer->SetMetric(metric);

  using ParametersType = MultiStartOptimizerv4TestMetric::ParametersType;

  const unsigned int spaceDimension = metric->GetNumberOfParameters();

  /*
   * Test 1
   */
  // We start not so far from  | 2 -2 |
  OptimizerType::ParametersListType parametersList = itkOptimizer->GetParametersList();
  for (int i = -3; i < 3; ++i)
  {
    for (int j = -3; j < 3; ++j)
    {
      ParametersType testPosition(spaceDimension);
      testPosition[0] = static_cast<double>(i);
      testPosition[1] = static_cast<double>(j);
      parametersList.push_back(testPosition);
    }
  }

  metric->SetParameters(parametersList[0]);
  itkOptimizer->SetParametersList(parametersList);
  // test the optimization
  std::cout << "Test optimization 1 without local optimizer:" << std::endl;
  if (MultiStartOptimizerv4RunTest(itkOptimizer) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }
  std::cout << "Test 1 passed." << std::endl;

  /*
   * Test 2
   */
  std::cout << "Test optimization 2: with local optimizer" << std::endl;
  itkOptimizer->InstantiateLocalOptimizer();
  parametersList.clear();
  for (int i = -99; i < 103; i += 100)
  {
    for (int j = -3; j < -2; ++j)
    {
      ParametersType testPosition(spaceDimension);
      testPosition[0] = static_cast<double>(i);
      testPosition[1] = static_cast<double>(j);
      parametersList.push_back(testPosition);
    }
  }
  metric->SetParameters(parametersList[0]);
  itkOptimizer->SetParametersList(parametersList);
  if (MultiStartOptimizerv4RunTest(itkOptimizer) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }
  std::cout << "Test 2 passed." << std::endl;

  /*
   * Test 3
   */
  std::cout << "Test optimization 3: with local optimizer passed by user" << std::endl;
  parametersList.clear();
  for (int i = 1; i < 2; ++i)
  {
    for (int j = -103; j < 99; j += 100)
    {
      ParametersType testPosition(spaceDimension);
      testPosition[0] = static_cast<double>(i);
      testPosition[1] = static_cast<double>(j);
      parametersList.push_back(testPosition);
    }
  }
  metric->SetParameters(parametersList[0]);
  itkOptimizer->SetParametersList(parametersList);
  OptimizerType::LocalOptimizerPointer optimizer = OptimizerType::LocalOptimizerType::New();
  optimizer->SetLearningRate(1.e-1);
  optimizer->SetNumberOfIterations(25);
  itkOptimizer->SetLocalOptimizer(optimizer);
  ITK_TEST_SET_GET_VALUE(optimizer, itkOptimizer->GetLocalOptimizer());
  if (MultiStartOptimizerv4RunTest(itkOptimizer) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }
  std::cout << "Test 3 passed." << std::endl;
  return EXIT_SUCCESS;
}
