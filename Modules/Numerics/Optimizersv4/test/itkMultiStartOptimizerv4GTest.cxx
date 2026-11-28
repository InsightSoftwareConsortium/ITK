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
#include "itkGradientDescentOptimizerv4.h"
#include "itkGTest.h"

#include <vector>

namespace
{
// f(x) = (x - 3)^2 + 1, which is positive, and throws for x > 500, as an image metric does
// when a start leaves too few samples in the moving image.
class ThrowingQuadraticMetric : public itk::ObjectToObjectMetricBase
{
public:
  using Self = ThrowingQuadraticMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(ThrowingQuadraticMetric);

  void
  Initialize() override
  {}

  MeasureType
  GetValue() const override
  {
    const double x = m_Parameters[0];
    if (x > 500)
    {
      itkExceptionMacro("start " << x << " is outside");
    }
    return (x - 3) * (x - 3) + 1;
  }

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    derivative.SetSize(1);
    derivative[0] = -2 * (m_Parameters[0] - 3);
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    value = this->GetValue();
    this->GetDerivative(derivative);
  }

  void
  UpdateTransformParameters(const DerivativeType & update, ParametersValueType factor) override
  {
    m_Parameters += update * factor;
  }

  NumberOfParametersType
  GetNumberOfParameters() const override
  {
    return 1;
  }

  NumberOfParametersType
  GetNumberOfLocalParameters() const override
  {
    return 1;
  }

  bool
  HasLocalSupport() const override
  {
    return false;
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

private:
  ThrowingQuadraticMetric() { m_Parameters.SetSize(1); }

  ParametersType m_Parameters;
};

using OptimizerType = itk::MultiStartOptimizerv4;

constexpr OptimizerType::MeasureType maximumValue = itk::NumericTraits<OptimizerType::MeasureType>::max();

class MultiStartOptimizerv4 : public ::testing::Test
{
protected:
  // Runs a multi-start search over the given values of x.
  void
  RunStarts(const std::vector<double> & starts)
  {
    OptimizerType::ParametersListType parametersList;
    for (const double x : starts)
    {
      OptimizerType::ParametersType parameters(1);
      parameters[0] = x;
      parametersList.push_back(parameters);
    }
    m_Optimizer->SetParametersList(parametersList);
    m_Optimizer->StartOptimization();
  }

  void
  SetUp() override
  {
    m_Optimizer->SetMetric(m_Metric);
  }

  ThrowingQuadraticMetric::Pointer m_Metric{ ThrowingQuadraticMetric::New() };
  OptimizerType::Pointer           m_Optimizer{ OptimizerType::New() };
};
} // namespace

TEST_F(MultiStartOptimizerv4, FailedFirstStartIsNotTheBest)
{
  RunStarts({ 1000, 3, 1 });

  EXPECT_EQ(m_Optimizer->GetBestParametersIndex(), 1u);
  EXPECT_EQ(m_Optimizer->GetBestParameters()[0], 3.0);
  // One value per start, in start order; the failed start gets the maximum value.
  const std::vector<double> expectedValues{ maximumValue, 1, 5 };
  EXPECT_EQ(m_Optimizer->GetMetricValuesList(), expectedValues);
}

TEST_F(MultiStartOptimizerv4, FailedLaterStartKeepsTheBest)
{
  RunStarts({ 2, 1000, 3 });

  EXPECT_EQ(m_Optimizer->GetBestParametersIndex(), 2u);
  const std::vector<double> expectedValues{ 2, maximumValue, 1 };
  EXPECT_EQ(m_Optimizer->GetMetricValuesList(), expectedValues);
}

TEST_F(MultiStartOptimizerv4, FailedStartIgnoresTheValueOfAnEarlierRun)
{
  // The first run ends with the value 1; a failed first start of the second run must not reuse it.
  RunStarts({ 3 });
  RunStarts({ 1000, 2 });

  EXPECT_EQ(m_Optimizer->GetBestParametersIndex(), 1u);
  EXPECT_EQ(m_Optimizer->GetBestParameters()[0], 2.0);
}

TEST_F(MultiStartOptimizerv4, AllFailedStartsGiveTheMaximumValue)
{
  RunStarts({ 1000, 2000 });

  EXPECT_EQ(m_Optimizer->GetBestParametersIndex(), 0u);
  const std::vector<double> expectedValues{ maximumValue, maximumValue };
  ASSERT_EQ(m_Optimizer->GetMetricValuesList(), expectedValues);
  EXPECT_EQ(m_Optimizer->GetMetricValuesList()[m_Optimizer->GetBestParametersIndex()], maximumValue);
}

TEST_F(MultiStartOptimizerv4, FailedLocalOptimizationIsNotTheBest)
{
  auto localOptimizer = itk::GradientDescentOptimizerv4::New();
  localOptimizer->SetLearningRate(0.25);
  localOptimizer->SetNumberOfIterations(20);
  localOptimizer->SetDoEstimateLearningRateOnce(false);
  localOptimizer->SetDoEstimateLearningRateAtEachIteration(false);
  m_Optimizer->SetLocalOptimizer(localOptimizer);

  // The local optimizer throws from its first metric evaluation at x = 1000.
  RunStarts({ 1000, 2 });

  EXPECT_EQ(m_Optimizer->GetBestParametersIndex(), 1u);
  EXPECT_NEAR(m_Optimizer->GetBestParameters()[0], 3.0, 1e-3);
  ASSERT_EQ(m_Optimizer->GetMetricValuesList().size(), 2u);
  EXPECT_EQ(m_Optimizer->GetMetricValuesList()[0], maximumValue);
  EXPECT_NEAR(m_Optimizer->GetMetricValuesList()[1], 1.0, 1e-6);
}
