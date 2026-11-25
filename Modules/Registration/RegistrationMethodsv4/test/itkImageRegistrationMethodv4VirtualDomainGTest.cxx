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
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkTranslationTransform.h"
#include "itkGTest.h"

#include <cmath>
#include <vector>

// With -fno-sanitize-recover=undefined, these fail if a path iterates the unallocated virtual image.
namespace
{
constexpr unsigned int Dimension = 2;
using ImageType = itk::Image<float, Dimension>;
using TransformType = itk::TranslationTransform<double, Dimension>;

ImageType::Pointer
MakeBlobImage(const double centerX, const ImageType::IndexType & start = {})
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(start, ImageType::SizeType::Filled(32)));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const double dx = it.GetIndex()[0] - start[0] - centerX;
    const double dy = it.GetIndex()[1] - start[1] - 16.0;
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

  const typename Superclass::SamplePointContainerType &
  DrawRandomSamples(const itk::SizeValueType numberOfSamples)
  {
    this->SetNumberOfRandomSamples(numberOfSamples);
    this->SampleVirtualDomainRandomly();
    return this->m_SamplePoints;
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

namespace
{
// Same geometry as MakeBlobImage, with a nonzero start index and non-unit spacing.
ImageType::Pointer
MakeOffsetBlobImage(const double centerX)
{
  auto                   image = MakeBlobImage(centerX, { { 5, 3 } });
  ImageType::SpacingType spacing;
  spacing[0] = 1.5;
  spacing[1] = 0.75;
  ImageType::PointType origin;
  origin[0] = -4.0;
  origin[1] = 9.0;
  image->SetSpacing(spacing);
  image->SetOrigin(origin);
  return image;
}

ImageType::Pointer
MakeAllocatedReference(const ImageType * geometry)
{
  auto reference = ImageType::New();
  reference->CopyInformation(geometry);
  reference->SetRegions(geometry->GetLargestPossibleRegion());
  reference->Allocate();
  return reference;
}
} // namespace

TEST(ImageRegistrationMethodv4VirtualDomain, RandomSamplesMatchAllocatedImageIterator)
{
  using RegistrationType = itk::ImageRegistrationMethodv4<ImageType, ImageType, TransformType>;
  using MetricType = itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType>;
  using ScalesEstimatorType = FixedSamplingScalesEstimator<MetricType>;

  const auto fixedImage = MakeOffsetBlobImage(16.0);
  const auto reference = MakeAllocatedReference(fixedImage);
  const auto region = reference->GetLargestPossibleRegion();
  ASSERT_NE(region.GetIndex()[0], 0);
  ASSERT_NE(region.GetIndex()[1], 0);

  {
    constexpr int          seed = 7;
    constexpr unsigned int sampleCount = 512;

    const auto metric = MetricType::New();
    const auto optimizer = itk::GradientDescentOptimizerv4::New();
    optimizer->SetNumberOfIterations(1);

    const auto registration = RegistrationType::New();
    registration->SetFixedImage(fixedImage);
    registration->SetMovingImage(MakeOffsetBlobImage(18.0));
    registration->SetMetric(metric);
    registration->SetOptimizer(optimizer);
    registration->SetNumberOfLevels(1);
    registration->SetMetricSamplingStrategy(RegistrationType::MetricSamplingStrategyEnum::RANDOM);
    registration->SetMetricSamplingPercentage(static_cast<double>(sampleCount) / region.GetNumberOfPixels());
    registration->MetricSamplingReinitializeSeed(seed);
    ASSERT_NO_THROW(registration->Update());
    ASSERT_EQ(metric->GetVirtualImage()->GetBufferPointer(), nullptr);

    // The perturbation generator takes the first seed and the index generator the next.
    const auto perturbation = itk::Statistics::MersenneTwisterRandomVariateGenerator::New();
    perturbation->SetSeed(seed);
    itk::ImageRandomConstIteratorWithIndex<ImageType> referenceIt(reference.GetPointer(), region);
    referenceIt.ReinitializeSeed(seed + 1);
    referenceIt.SetNumberOfSamples(sampleCount);
    referenceIt.GoToBegin();

    const auto sampledPoints = metric->GetVirtualSampledPointSet();
    ASSERT_EQ(sampledPoints->GetNumberOfPoints(), sampleCount);
    const auto thirdOfSpacing = reference->GetSpacing() / 3.0;
    for (itk::IdentifierType i = 0; i < sampleCount; ++i, ++referenceIt)
    {
      ASSERT_TRUE(region.IsInside(referenceIt.GetIndex())) << "sample " << i;
      ImageType::PointType expected;
      reference->TransformIndexToPhysicalPoint(referenceIt.GetIndex(), expected);
      for (unsigned int d = 0; d < Dimension; ++d)
      {
        expected[d] += perturbation->GetNormalVariate() * thirdOfSpacing[d];
      }
      const auto actual = sampledPoints->GetPoint(i);
      EXPECT_NEAR(actual[0], expected[0], 1e-4) << "sample " << i;
      EXPECT_NEAR(actual[1], expected[1], 1e-4) << "sample " << i;
    }
  }

  {
    constexpr itk::SizeValueType sampleCount = 200;

    const auto metric = MetricType::New();
    metric->SetFixedImage(fixedImage);
    metric->SetMovingImage(MakeOffsetBlobImage(18.0));
    metric->SetMovingTransform(TransformType::New());
    metric->Initialize();
    ASSERT_EQ(metric->GetVirtualRegion(), region);

    const auto scalesEstimator = ScalesEstimatorType::New();
    scalesEstimator->SetMetric(metric);

    // Generators created by New() take sequential seeds, so resetting makes both draws start from the same one.
    itk::Statistics::MersenneTwisterRandomVariateGenerator::ResetNextSeed();
    itk::ImageRandomConstIteratorWithIndex<ImageType> referenceIt(reference.GetPointer(), region);
    referenceIt.SetNumberOfSamples(sampleCount);
    referenceIt.GoToBegin();

    itk::Statistics::MersenneTwisterRandomVariateGenerator::ResetNextSeed();
    const auto & sampledPoints = scalesEstimator->DrawRandomSamples(sampleCount);
    ASSERT_EQ(sampledPoints.size(), sampleCount);
    for (itk::SizeValueType i = 0; i < sampleCount; ++i, ++referenceIt)
    {
      ASSERT_TRUE(region.IsInside(referenceIt.GetIndex())) << "sample " << i;
      ImageType::PointType expected;
      reference->TransformIndexToPhysicalPoint(referenceIt.GetIndex(), expected);
      EXPECT_NEAR(sampledPoints[i][0], expected[0], 1e-4) << "sample " << i;
      EXPECT_NEAR(sampledPoints[i][1], expected[1], 1e-4) << "sample " << i;
    }
  }
}
