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

#include "itkIdentityTransform.h"
#include "itkObjectToObjectMetric.h"
#include <gtest/gtest.h>
#include <limits>

namespace
{
class CountTransform : public itk::IdentityTransform<double, 3>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CountTransform);
  using Self = CountTransform;
  using Superclass = itk::IdentityTransform<double, 3>;
  using Pointer = itk::SmartPointer<Self>;
  using NumberOfParametersType = Superclass::NumberOfParametersType;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(CountTransform);

  void
  SetCount(NumberOfParametersType count)
  {
    m_Count = count;
  }

  NumberOfParametersType
  GetNumberOfParameters() const override
  {
    return m_Count;
  }

protected:
  CountTransform() = default;
  ~CountTransform() override = default;

private:
  NumberOfParametersType m_Count{};
};

class CountMetric : public itk::ObjectToObjectMetric<3, 3>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CountMetric);
  using Self = CountMetric;
  using Superclass = itk::ObjectToObjectMetric<3, 3>;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(CountMetric);

  MeasureType
  GetValue() const override
  {
    return 0.0;
  }

  void
  GetDerivative(DerivativeType & derivative) const override
  {
    derivative.SetSize(0);
  }

  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override
  {
    value = this->GetValue();
    this->GetDerivative(derivative);
  }

  bool
  SupportsArbitraryVirtualDomainSamples() const override
  {
    return false;
  }

protected:
  CountMetric() = default;
  ~CountMetric() override = default;
};
} // namespace

TEST(ObjectToObjectMetric, PreservesRepresentableParameterCounts)
{
  const auto transform = CountTransform::New();
  const auto metric = CountMetric::New();
  metric->SetMovingTransform(transform);
  using CountType = CountMetric::NumberOfParametersType;
  for (const CountType count : { CountType{ 0 }, CountType{ 17 }, std::numeric_limits<CountType>::max() })
  {
    transform->SetCount(count);
    EXPECT_EQ(metric->GetNumberOfParameters(), count);
  }
}

TEST(ObjectToObjectMetric, RejectsParameterCountOverflow)
{
  using TransformCountType = CountTransform::NumberOfParametersType;
  using MetricCountType = CountMetric::NumberOfParametersType;
  if constexpr (std::numeric_limits<TransformCountType>::digits <= std::numeric_limits<MetricCountType>::digits)
  {
    GTEST_SKIP() << "The transform count type is not wider than the metric count type";
  }
  else
  {
    const auto transform = CountTransform::New();
    const auto metric = CountMetric::New();
    metric->SetMovingTransform(transform);
    const auto maximum = static_cast<TransformCountType>(std::numeric_limits<MetricCountType>::max());
    transform->SetCount(maximum + TransformCountType{ 1 });
    EXPECT_THROW(metric->GetNumberOfParameters(), itk::ExceptionObject);
    transform->SetCount(static_cast<TransformCountType>(5936642280ULL));
    EXPECT_THROW(metric->GetNumberOfParameters(), itk::ExceptionObject);
  }
}
