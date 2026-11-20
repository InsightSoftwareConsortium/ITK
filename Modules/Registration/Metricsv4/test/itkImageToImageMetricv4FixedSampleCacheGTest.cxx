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

// Compares an evaluated metric, which keeps its fixed sample values, with a new metric after each change.

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkCorrelationImageToImageMetricv4.h"
#include "itkDemonsImageToImageMetricv4.h"
#include "itkDisplacementFieldTransform.h"
#include "itkGTest.h"
#include "itkImageMaskSpatialObject.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"

#include <cmath>
#include <type_traits>

namespace
{
constexpr unsigned int Dimension = 3;
using ImageType = itk::Image<float, Dimension>;
using AffineTransformType = itk::AffineTransform<double, Dimension>;
using FieldTransformType = itk::DisplacementFieldTransform<double, Dimension>;
using MeanSquaresMetricType = itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType>;
using MattesMetricType = itk::MattesMutualInformationImageToImageMetricv4<ImageType, ImageType>;
using DemonsMetricType = itk::DemonsImageToImageMetricv4<ImageType, ImageType>;

// Demons needs a displacement field moving transform; the other metrics get an affine one.
template <typename TMetric>
constexpr bool UsesFieldTransform = std::is_same_v<TMetric, DemonsMetricType>;

FieldTransformType::Pointer
MakeFieldTransform()
{
  auto field = FieldTransformType::DisplacementFieldType::New();
  field->SetRegions(ImageType::SizeType::Filled(20));
  field->AllocateInitialized();
  auto transform = FieldTransformType::New();
  transform->SetDisplacementField(field);
  return transform;
}

ImageType::Pointer
MakeImage(const double phase)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(20));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const auto index = it.GetIndex();
    it.Set(static_cast<float>(10 * std::sin(0.4 * index[0] + phase) * std::cos(0.3 * index[1]) + 0.5 * index[2]));
  }
  return image;
}

// Off-grid samples, a few of them outside the image.
template <typename TPointSet>
typename TPointSet::Pointer
MakeSamples()
{
  auto points = TPointSet::New();
  for (unsigned int id = 0; id < 1500; ++id)
  {
    typename TPointSet::PointType point;
    point[0] = std::fmod(id * 7.31, 21.0) - 0.5;
    point[1] = std::fmod(id * 3.17, 20.0) - 0.25;
    point[2] = std::fmod(id * 5.53, 19.5);
    points->SetPoint(id, point);
  }
  return points;
}

template <typename TMetric>
typename TMetric::Pointer
MakeMetric(const ImageType * fixedImage, const ImageType * movingImage, const itk::ThreadIdType workUnits)
{
  auto metric = TMetric::New();
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);
  if constexpr (UsesFieldTransform<TMetric>)
  {
    metric->SetMovingTransform(MakeFieldTransform());
  }
  else
  {
    metric->SetMovingTransform(AffineTransformType::New());
  }
  metric->SetFixedSampledPointSet(MakeSamples<typename TMetric::FixedSampledPointSetType>());
  metric->SetUseSampledPointSet(true);
  metric->SetMaximumNumberOfWorkUnits(workUnits);
  return metric;
}

// Virtual-domain samples, so that the fixed transform changes only where the fixed image is read.
template <typename TMetric>
void
UseVirtualSamples(TMetric & metric)
{
  metric.SetVirtualSampledPointSet(MakeSamples<typename TMetric::VirtualPointSetType>());
  metric.SetUseVirtualSampledPointSet(true);
}

// Mattes sums work units in finishing order, and field transforms race (#7234), so these vary across runs.
template <typename TMetric>
constexpr bool DerivativeIsRepeatable = !std::is_same_v<TMetric, MattesMetricType> && !UsesFieldTransform<TMetric>;

// Moving transform parameters: the identity, changed a little by step.
template <typename TMetric>
typename TMetric::ParametersType
MovingParameters(const unsigned int step)
{
  typename TMetric::ParametersType parameters;
  if constexpr (UsesFieldTransform<TMetric>)
  {
    parameters = MakeFieldTransform()->GetParameters();
  }
  else
  {
    parameters = AffineTransformType::New()->GetParameters();
  }
  for (unsigned int i = 0; i < parameters.size(); ++i)
  {
    parameters[i] += (i < 9 ? 0.01 : 0.4) * std::sin(1.0 + step + i);
  }
  return parameters;
}

template <typename TMetric>
void
Evaluate(TMetric &                          metric,
         const unsigned int                 step,
         typename TMetric::MeasureType &    value,
         typename TMetric::DerivativeType & derivative)
{
  auto parameters = MovingParameters<TMetric>(step);
  metric.SetParameters(parameters);
  metric.GetValueAndDerivative(value, derivative);
}

// Expects the value (and, if compareDerivative, the derivative) of a new metric from makeMetric at step.
template <typename TMetric, typename TMakeMetric>
void
ExpectSameAsNewMetric(TMetric &           metric,
                      const unsigned int  step,
                      const TMakeMetric & makeMetric,
                      const bool          compareDerivative = true)
{
  typename TMetric::MeasureType    value{};
  typename TMetric::DerivativeType derivative;
  Evaluate(metric, step, value, derivative);

  auto newMetric = makeMetric();
  newMetric->Initialize();
  typename TMetric::MeasureType    expectedValue{};
  typename TMetric::DerivativeType expectedDerivative;
  Evaluate(*newMetric, step, expectedValue, expectedDerivative);

  EXPECT_EQ(value, expectedValue);
  if (compareDerivative)
  {
    EXPECT_EQ(derivative, expectedDerivative);
  }
}
} // namespace

template <typename TMetric>
class ImageToImageMetricv4FixedSampleCache : public ::testing::Test
{};

using MetricTypes = ::testing::Types<MeanSquaresMetricType,
                                     MattesMetricType,
                                     DemonsMetricType,
                                     itk::CorrelationImageToImageMetricv4<ImageType, ImageType>,
                                     itk::JointHistogramMutualInformationImageToImageMetricv4<ImageType, ImageType>>;
TYPED_TEST_SUITE(ImageToImageMetricv4FixedSampleCache, MetricTypes);

TYPED_TEST(ImageToImageMetricv4FixedSampleCache, RepeatedEvaluationsMatchANewMetric)
{
  using MetricType = TypeParam;
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  for (const itk::ThreadIdType workUnits : { 1, 4 })
  {
    const auto makeMetric = [&]() { return MakeMetric<MetricType>(fixedImage, movingImage, workUnits); };
    auto       metric = makeMetric();
    metric->Initialize();

    // GetValue() first, then GetValueAndDerivative() at other parameters.
    auto parameters = MovingParameters<MetricType>(1);
    metric->SetParameters(parameters);
    const typename MetricType::MeasureType firstValue = metric->GetValue();
    typename MetricType::MeasureType       value{};
    typename MetricType::DerivativeType    derivative;
    Evaluate(*metric, 2, value, derivative);

    ExpectSameAsNewMetric(*metric, 3, makeMetric, workUnits == 1 || DerivativeIsRepeatable<MetricType>);

    auto newMetric = makeMetric();
    newMetric->Initialize();
    newMetric->SetParameters(parameters);
    EXPECT_EQ(firstValue, newMetric->GetValue());
  }
}

TEST(ImageToImageMetricv4FixedSampleCache, FollowsTheFixedTransformParameters)
{
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  for (const itk::ThreadIdType workUnits : { 1, 4 })
  {
    auto       fixedTransform = AffineTransformType::New();
    const auto makeMetric = [&]() {
      // Demons reads the fixed gradient at the mapped points; see DerivativeIsRepeatable.
      auto metric = MakeMetric<DemonsMetricType>(fixedImage, movingImage, workUnits);
      metric->SetFixedTransform(fixedTransform);
      UseVirtualSamples(*metric);
      return metric;
    };
    auto metric = makeMetric();
    metric->Initialize();
    ExpectSameAsNewMetric(*metric, 1, makeMetric, workUnits == 1);
    // Unchanged: reads the kept mapped points.
    ExpectSameAsNewMetric(*metric, 2, makeMetric, workUnits == 1);

    fixedTransform->Translate(AffineTransformType::OutputVectorType(0.7));
    ExpectSameAsNewMetric(*metric, 3, makeMetric, workUnits == 1);
    ExpectSameAsNewMetric(*metric, 4, makeMetric, workUnits == 1);
  }
}

TEST(ImageToImageMetricv4FixedSampleCache, FollowsATransformInsideACompositeFixedTransform)
{
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  for (const itk::ThreadIdType workUnits : { 1, 4 })
  {
    auto affine = AffineTransformType::New();
    auto composite = itk::CompositeTransform<double, Dimension>::New();
    composite->AddTransform(affine);
    const auto makeMetric = [&]() {
      // Demons reads the fixed gradient at the mapped points; see DerivativeIsRepeatable.
      auto metric = MakeMetric<DemonsMetricType>(fixedImage, movingImage, workUnits);
      metric->SetFixedTransform(composite);
      UseVirtualSamples(*metric);
      return metric;
    };
    auto metric = makeMetric();
    metric->Initialize();
    ExpectSameAsNewMetric(*metric, 1, makeMetric, workUnits == 1);
    // Unchanged: reads the kept mapped points.
    ExpectSameAsNewMetric(*metric, 2, makeMetric, workUnits == 1);

    // Modifying the affine transform does not change the modified time of the composite transform.
    affine->Translate(AffineTransformType::OutputVectorType(-0.6));
    ExpectSameAsNewMetric(*metric, 3, makeMetric, workUnits == 1);
    ExpectSameAsNewMetric(*metric, 4, makeMetric, workUnits == 1);
  }
}

TEST(ImageToImageMetricv4FixedSampleCache, FollowsTheFixedImageValues)
{
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  const auto makeMetric = [&]() { return MakeMetric<MeanSquaresMetricType>(fixedImage, movingImage, 1); };
  auto       metric = makeMetric();
  metric->Initialize();
  ExpectSameAsNewMetric(*metric, 1, makeMetric);

  for (itk::ImageRegionIteratorWithIndex<ImageType> it(fixedImage, fixedImage->GetBufferedRegion()); !it.IsAtEnd();
       ++it)
  {
    it.Set(it.Get() * 1.5F);
  }
  fixedImage->Modified();
  ExpectSameAsNewMetric(*metric, 2, makeMetric);
}

TEST(ImageToImageMetricv4FixedSampleCache, KeepsTheFixedImageValuesUntilTheImageIsModified)
{
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  const auto makeMetric = [&]() { return MakeMetric<MeanSquaresMetricType>(fixedImage, movingImage, 1); };
  auto       metric = makeMetric();
  metric->Initialize();
  ExpectSameAsNewMetric(*metric, 1, makeMetric);

  auto unchangedMetric = makeMetric();
  unchangedMetric->Initialize();
  MeanSquaresMetricType::MeasureType    unchangedValue{};
  MeanSquaresMetricType::DerivativeType unchangedDerivative;
  Evaluate(*unchangedMetric, 2, unchangedValue, unchangedDerivative);

  // An edit of the buffer that does not call Modified() is not seen: the kept values are used.
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(fixedImage, fixedImage->GetBufferedRegion()); !it.IsAtEnd();
       ++it)
  {
    it.Set(it.Get() * 1.5F);
  }
  MeanSquaresMetricType::MeasureType    value{};
  MeanSquaresMetricType::DerivativeType derivative;
  Evaluate(*metric, 2, value, derivative);
  EXPECT_EQ(value, unchangedValue);
  EXPECT_EQ(derivative, unchangedDerivative);

  fixedImage->Modified();
  ExpectSameAsNewMetric(*metric, 3, makeMetric);
}

TEST(ImageToImageMetricv4FixedSampleCache, FollowsTheFixedMask)
{
  using MaskType = itk::ImageMaskSpatialObject<Dimension>;
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  auto       maskImage = MaskType::ImageType::New();
  maskImage->SetRegions(ImageType::SizeType::Filled(20));
  maskImage->AllocateInitialized();
  for (itk::ImageRegionIteratorWithIndex<MaskType::ImageType> it(maskImage, maskImage->GetBufferedRegion());
       !it.IsAtEnd();
       ++it)
  {
    it.Set(it.GetIndex()[0] < 15 ? 1 : 0);
  }
  auto mask = MaskType::New();
  mask->SetImage(maskImage);
  mask->Update();
  const auto makeMetric = [&]() {
    auto metric = MakeMetric<MeanSquaresMetricType>(fixedImage, movingImage, 1);
    metric->SetFixedImageMask(mask);
    return metric;
  };
  auto metric = makeMetric();
  metric->Initialize();
  ExpectSameAsNewMetric(*metric, 1, makeMetric);

  for (itk::ImageRegionIteratorWithIndex<MaskType::ImageType> it(maskImage, maskImage->GetBufferedRegion());
       !it.IsAtEnd();
       ++it)
  {
    it.Set(it.GetIndex()[0] < 8 ? 1 : 0);
  }
  maskImage->Modified();
  mask->Update();
  ExpectSameAsNewMetric(*metric, 2, makeMetric);
}

TEST(ImageToImageMetricv4FixedSampleCache, FollowsTheSampledPoints)
{
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  const auto virtualPoints = MakeSamples<MeanSquaresMetricType::VirtualPointSetType>();
  const auto makeMetric = [&]() {
    auto metric = MakeMetric<MeanSquaresMetricType>(fixedImage, movingImage, 1);
    metric->SetVirtualSampledPointSet(virtualPoints);
    metric->SetUseVirtualSampledPointSet(true);
    return metric;
  };
  auto metric = makeMetric();
  metric->Initialize();
  ExpectSameAsNewMetric(*metric, 1, makeMetric);

  for (unsigned int id = 0; id < virtualPoints->GetNumberOfPoints(); id += 3)
  {
    auto point = virtualPoints->GetPoint(id);
    point[0] += 0.37;
    virtualPoints->SetPoint(id, point);
  }
  ExpectSameAsNewMetric(*metric, 2, makeMetric);
}

TEST(ImageToImageMetricv4FixedSampleCache, IsNotUsedWithADisplacementFieldFixedTransform)
{
  const auto fixedImage = MakeImage(0.0);
  const auto movingImage = MakeImage(0.3);
  auto       field = FieldTransformType::DisplacementFieldType::New();
  field->SetRegions(ImageType::SizeType::Filled(20));
  field->AllocateInitialized();
  auto fieldTransform = FieldTransformType::New();
  fieldTransform->SetDisplacementField(field);
  const auto makeMetric = [&]() {
    auto metric = MakeMetric<MeanSquaresMetricType>(fixedImage, movingImage, 1);
    metric->SetFixedTransform(fieldTransform);
    UseVirtualSamples(*metric);
    return metric;
  };
  auto metric = makeMetric();
  metric->Initialize();
  ExpectSameAsNewMetric(*metric, 1, makeMetric);

  // A displacement field can change in place, without changing any modified time.
  field->FillBuffer(FieldTransformType::DisplacementFieldType::PixelType(0.8));
  ExpectSameAsNewMetric(*metric, 2, makeMetric);
}
