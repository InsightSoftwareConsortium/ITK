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

#include "itkCommand.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkObjectToObjectMetricBase.h"
#include "itkRegularStepGradientDescentOptimizerv4.h"

#include "itkGTest.h"


namespace
{

// Quadratic 1/2 x^T A x - b^T x with A = [[3,2],[2,6]], b = [2,-8].
// Minimum at x = [2, -2]. Used by Optimizersv4 tests for decades.
class QuadraticMetric : public itk::ObjectToObjectMetricBase
{
public:
  using Self = QuadraticMetric;
  using Superclass = itk::ObjectToObjectMetricBase;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(QuadraticMetric);

  using ParametersType = Superclass::ParametersType;
  using ParametersValueType = Superclass::ParametersValueType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;
  static constexpr unsigned int SpaceDimension = 2;

  QuadraticMetric()
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
    MeasureType v = NAN;
    GetValueAndDerivative(v, derivative);
  }

  static MeasureType
  EvaluateAt(const ParametersType & p)
  {
    const double x = p[0];
    const double y = p[1];
    return 0.5 * (3 * x * x + 4 * x * y + 6 * y * y) - 2 * x + 8 * y;
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    if (derivative.Size() != SpaceDimension)
    {
      derivative.SetSize(SpaceDimension);
    }
    value = EvaluateAt(m_Parameters);
    derivative[0] = -(3 * m_Parameters[0] + 2 * m_Parameters[1] - 2);
    derivative[1] = -(2 * m_Parameters[0] + 6 * m_Parameters[1] + 8);
  }

  MeasureType
  GetValue() const override
  {
    return EvaluateAt(m_Parameters);
  }

  void
  UpdateTransformParameters(const DerivativeType & update, ParametersValueType factor) override
  {
    for (unsigned int i = 0; i < SpaceDimension; ++i)
    {
      m_Parameters[i] += factor * update[i];
    }
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
  void
  SetParameters(ParametersType & p) override
  {
    m_Parameters = p;
  }
  const ParametersType &
  GetParameters() const override
  {
    return m_Parameters;
  }
  bool
  HasLocalSupport() const override
  {
    return false;
  }

private:
  ParametersType m_Parameters;
};


// Captures (value, position) pairs at every IterationEvent.
template <typename TOptimizer>
class IterationCapture : public itk::Command
{
public:
  using Self = IterationCapture;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

  struct Record
  {
    typename TOptimizer::MeasureType    reported_value;
    typename TOptimizer::ParametersType reported_position;
  };
  std::vector<Record> records;

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute(const_cast<const itk::Object *>(caller), event);
  }

  void
  Execute(const itk::Object * caller, const itk::EventObject & event) override
  {
    if (!itk::IterationEvent().CheckEvent(&event))
    {
      return;
    }
    const auto * opt = dynamic_cast<const TOptimizer *>(caller);
    ASSERT_NE(opt, nullptr);
    records.push_back({ opt->GetCurrentMetricValue(), opt->GetCurrentPosition() });
  }
};

template <typename TOptimizer>
void
RunAndAssertObserverConsistency()
{
  auto                            metric = QuadraticMetric::New();
  QuadraticMetric::ParametersType initial(2);
  initial[0] = 100.0;
  initial[1] = -100.0;
  metric->SetParameters(initial);

  auto opt = TOptimizer::New();
  opt->SetMetric(metric);
  opt->SetNumberOfIterations(5);
  opt->SetLearningRate(0.01);

  auto capture = IterationCapture<TOptimizer>::New();
  opt->AddObserver(itk::IterationEvent(), capture);

  opt->StartOptimization();

  ASSERT_GT(capture->records.size(), 0u);

  for (size_t i = 0; i < capture->records.size(); ++i)
  {
    const auto & r = capture->records[i];
    const auto   actual_at_reported_pos = QuadraticMetric::EvaluateAt(r.reported_position);
    EXPECT_NEAR(r.reported_value, actual_at_reported_pos, 1e-6)
      << "Iteration " << i << ": observer reported value " << r.reported_value << " at position " << r.reported_position
      << " but metric at that position is " << actual_at_reported_pos
      << " — observer's (value, position) pair is inconsistent (issue #2570).";
  }
}

} // namespace


// Regression guard for issue #2570: at every IterationEvent, the value reported by
// GetCurrentMetricValue() must equal the metric evaluated at GetCurrentPosition().
TEST(GradientDescentOptimizerv4, ObserverReportsConsistentValueAndPosition)
{
  RunAndAssertObserverConsistency<itk::GradientDescentOptimizerv4>();
}

TEST(RegularStepGradientDescentOptimizerv4, ObserverReportsConsistentValueAndPosition)
{
  RunAndAssertObserverConsistency<itk::RegularStepGradientDescentOptimizerv4<double>>();
}
