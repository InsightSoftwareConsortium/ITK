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

// First include the header file to be tested:
#include "itkObjectToObjectMetric.h"

#include "itkGTest.h"
#include "itkAmoebaOptimizerv4.h"
#include "itkIdentityTransform.h"
#include "itkLBFGSBOptimizerv4.h"
#include "itkLBFGSOptimizerv4.h"
#include <cstdint>
#include <limits>
#include <type_traits>

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

// A dense displacement field can have more than 2^32 parameters; the metric must report all of them (#6949).
TEST(ObjectToObjectMetric, PreservesParameterCountsAbove32Bits)
{
  static_assert(std::is_same_v<CountMetric::NumberOfParametersType, CountTransform::NumberOfParametersType>);
  const auto transform = CountTransform::New();
  const auto metric = CountMetric::New();
  metric->SetMovingTransform(transform);
  using CountType = CountMetric::NumberOfParametersType;
  if constexpr (std::numeric_limits<CountType>::digits <= 32)
  {
    GTEST_SKIP() << "IdentifierType is 32 bits in this build";
  }
  else
  {
    const CountType count = CountType{ std::numeric_limits<uint32_t>::max() } + 4;
    transform->SetCount(count);
    EXPECT_EQ(metric->GetNumberOfParameters(), count);
  }
}

// The vnl-based optimizers size their vnl objects with unsigned int; a larger count must throw, not truncate.
TEST(ObjectToObjectMetric, VnlOptimizersRejectParameterCountsAbove32Bits)
{
  const auto transform = CountTransform::New();
  const auto metric = CountMetric::New();
  metric->SetMovingTransform(transform);
  using CountType = CountMetric::NumberOfParametersType;
  if constexpr (std::numeric_limits<CountType>::digits <= 32)
  {
    GTEST_SKIP() << "IdentifierType is 32 bits in this build";
  }
  else
  {
    transform->SetCount(CountType{ std::numeric_limits<uint32_t>::max() } + 4);
    EXPECT_THROW(itk::LBFGSOptimizerv4::New()->SetMetric(metric), itk::RangeError);
    EXPECT_THROW(itk::LBFGSBOptimizerv4::New()->SetMetric(metric), itk::RangeError);
    EXPECT_THROW(itk::AmoebaOptimizerv4::New()->SetMetric(metric), itk::RangeError);

    transform->SetCount(17);
    EXPECT_NO_THROW(itk::LBFGSOptimizerv4::New()->SetMetric(metric));
    EXPECT_NO_THROW(itk::AmoebaOptimizerv4::New()->SetMetric(metric));
  }
}
