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
#include "itkImageRegistrationMethodv4.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkTranslationTransform.h"
#include "itkGTest.h"

#include <cmath>

// With -fno-sanitize-recover=undefined, these fail if a path iterates the unallocated virtual image.
namespace
{
constexpr unsigned int Dimension = 2;
using ImageType = itk::Image<float, Dimension>;
using TransformType = itk::TranslationTransform<double, Dimension>;

ImageType::Pointer
MakeBlobImage(const double centerX)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(ImageType::SizeType::Filled(32)));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const double dx = it.GetIndex()[0] - centerX;
    const double dy = it.GetIndex()[1] - 16.0;
    it.Set(static_cast<float>(100.0 * std::exp(-(dx * dx + dy * dy) / 50.0)));
  }
  return image;
}

template <typename TMetric>
void
CheckMetricOnUnallocatedVirtualImage(TMetric * metric)
{
  metric->SetFixedImage(MakeBlobImage(16.0));
  metric->SetMovingImage(MakeBlobImage(18.0));
  metric->SetMovingTransform(TransformType::New());
  metric->Initialize();
  ASSERT_EQ(metric->GetVirtualImage()->GetBufferPointer(), nullptr);

  typename TMetric::MeasureType    value{};
  typename TMetric::DerivativeType derivative;
  metric->GetValueAndDerivative(value, derivative);
  EXPECT_TRUE(std::isfinite(value));
  ASSERT_EQ(derivative.size(), Dimension);
  EXPECT_NE(derivative[0], 0.0);
}

// Forces one sampling strategy; the base class otherwise picks it from the transform type.
template <typename TMetric>
class FixedSamplingScalesEstimator : public itk::RegistrationParameterScalesFromPhysicalShift<TMetric>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FixedSamplingScalesEstimator);

  using Self = FixedSamplingScalesEstimator;
  using Superclass = itk::RegistrationParameterScalesFromPhysicalShift<TMetric>;
  using Pointer = itk::SmartPointer<Self>;
  using typename Superclass::SamplingStrategyType;

  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(FixedSamplingScalesEstimator);

  void
  SetForcedSamplingStrategy(const SamplingStrategyType strategy)
  {
    m_ForcedSamplingStrategy = strategy;
  }

  void
  SetScalesSamplingStrategy() override
  {
    this->SetSamplingStrategy(m_ForcedSamplingStrategy);
  }

  void
  SetStepScaleSamplingStrategy() override
  {
    this->SetSamplingStrategy(m_ForcedSamplingStrategy);
  }

protected:
  FixedSamplingScalesEstimator() = default;
  ~FixedSamplingScalesEstimator() override = default;

private:
  SamplingStrategyType m_ForcedSamplingStrategy{ SamplingStrategyType::RandomSampling };
};
} // namespace

TEST(ImageRegistrationMethodv4VirtualDomain, MeanSquaresMetric)
{
  const auto metric = itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType>::New();
  CheckMetricOnUnallocatedVirtualImage(metric.GetPointer());
}

TEST(ImageRegistrationMethodv4VirtualDomain, JointHistogramMutualInformationMetric)
{
  const auto metric = itk::JointHistogramMutualInformationImageToImageMetricv4<ImageType, ImageType>::New();
  CheckMetricOnUnallocatedVirtualImage(metric.GetPointer());
}

TEST(ImageRegistrationMethodv4VirtualDomain, ANTSNeighborhoodCorrelationMetric)
{
  const auto metric = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<ImageType, ImageType>::New();
  metric->SetRadius(itk::Size<Dimension>::Filled(2));
  CheckMetricOnUnallocatedVirtualImage(metric.GetPointer());
}

TEST(ImageRegistrationMethodv4VirtualDomain, SamplingStrategiesAndScalesEstimator)
{
  using RegistrationType = itk::ImageRegistrationMethodv4<ImageType, ImageType, TransformType>;
  using MetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<ImageType, ImageType>;
  using ScalesEstimatorType = FixedSamplingScalesEstimator<MetricType>;

  for (const auto sampling : { RegistrationType::MetricSamplingStrategyEnum::NONE,
                               RegistrationType::MetricSamplingStrategyEnum::REGULAR,
                               RegistrationType::MetricSamplingStrategyEnum::RANDOM })
  {
    for (const auto scalesSampling : { ScalesEstimatorType::SamplingStrategyType::CentralRegionSampling,
                                       ScalesEstimatorType::SamplingStrategyType::RandomSampling })
    {
      const auto metric = MetricType::New();
      metric->SetRadius(itk::Size<Dimension>::Filled(2));

      const auto scalesEstimator = ScalesEstimatorType::New();
      scalesEstimator->SetMetric(metric);
      scalesEstimator->SetForcedSamplingStrategy(scalesSampling);

      const auto optimizer = itk::GradientDescentOptimizerv4::New();
      optimizer->SetNumberOfIterations(3);
      optimizer->SetScalesEstimator(scalesEstimator);
      optimizer->SetDoEstimateLearningRateOnce(true);

      const auto registration = RegistrationType::New();
      registration->SetFixedImage(MakeBlobImage(16.0));
      registration->SetMovingImage(MakeBlobImage(18.0));
      registration->SetMetric(metric);
      registration->SetOptimizer(optimizer);
      registration->SetNumberOfLevels(1);
      registration->SetMetricSamplingStrategy(sampling);
      registration->SetMetricSamplingPercentage(0.5);
      registration->MetricSamplingReinitializeSeed(7);

      ASSERT_NO_THROW(registration->Update());
      EXPECT_EQ(metric->GetVirtualImage()->GetBufferPointer(), nullptr);
      const auto & parameters = registration->GetTransform()->GetParameters();
      EXPECT_TRUE(std::isfinite(parameters[0]));
      EXPECT_TRUE(std::isfinite(parameters[1]));
      EXPECT_GE(optimizer->GetCurrentIteration(), 1u);
    }
  }
}
