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

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkBSplineSyNImageRegistrationMethod.h"
#include "itkDisplacementFieldTransformParametersAdaptor.h"
#include "itkImageMaskSpatialObject.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkNthElementImageAdaptor.h"
#include "itkResampleImageFilter.h"
#include "itkSyNImageRegistrationMethod.h"
#include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.h"
#include "itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor.h"
#include "itkTimeVaryingVelocityFieldImageRegistrationMethodv4.h"
#include "itkTimeVaryingVelocityFieldTransform.h"
#include "itkTimeVaryingVelocityFieldTransformParametersAdaptor.h"
#include "itkTranslationTransform.h"
#include "itkGTest.h"

#include <algorithm>
#include <cmath>
#include <optional>
#include <type_traits>
#include <vector>

namespace
{
constexpr unsigned int Dimension = 2;
using ImageType = itk::Image<double, Dimension>;
using FieldType = itk::Image<itk::Vector<double, Dimension>, Dimension>;
using MetricType = itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType>;

using TimeVaryingBSplineRegistrationType =
  itk::TimeVaryingBSplineVelocityFieldImageRegistrationMethod<ImageType, ImageType>;

constexpr itk::SizeValueType imageSize = 32;
constexpr unsigned int       numberOfLevels = 2;

ImageType::Pointer
MakeFixedImage()
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(imageSize));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const double x = it.GetIndex()[0] - 0.5 * imageSize;
    const double y = it.GetIndex()[1] - (0.5 * imageSize - 1.0);
    it.Set(100.0 * std::exp(-(x * x + 2.0 * y * y) / 60.0));
  }
  return image;
}

ImageType::Pointer
MakeMovingImage(const ImageType * fixed)
{
  auto warp = FieldType::New();
  warp->CopyInformation(fixed);
  warp->SetRegions(fixed->GetBufferedRegion());
  warp->Allocate();
  for (itk::ImageRegionIteratorWithIndex<FieldType> it(warp, warp->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    FieldType::PixelType v;
    v[0] = 1.5 * std::sin(2.0 * itk::Math::pi * it.GetIndex()[1] / imageSize);
    v[1] = 1.0 * std::cos(2.0 * itk::Math::pi * it.GetIndex()[0] / imageSize);
    it.Set(v);
  }
  auto warpTransform = itk::DisplacementFieldTransform<double, Dimension>::New();
  warpTransform->SetDisplacementField(warp);
  auto resampler = itk::ResampleImageFilter<ImageType, ImageType, double, double>::New();
  resampler->SetInput(fixed);
  resampler->SetTransform(warpTransform);
  resampler->SetReferenceImage(fixed);
  resampler->UseReferenceImageOn();
  resampler->Update();
  ImageType::Pointer moving = resampler->GetOutput();
  moving->DisconnectPipeline();
  return moving;
}

// A mask of the central half of the image, so that masking changes the result.
itk::ImageMaskSpatialObject<Dimension>::Pointer
MakeMask(const ImageType * fixed)
{
  using MaskImageType = itk::ImageMaskSpatialObject<Dimension>::ImageType;
  auto maskImage = MaskImageType::New();
  maskImage->CopyInformation(fixed);
  maskImage->SetRegions(fixed->GetBufferedRegion());
  maskImage->AllocateInitialized();
  for (itk::ImageRegionIteratorWithIndex<MaskImageType> it(maskImage, maskImage->GetBufferedRegion()); !it.IsAtEnd();
       ++it)
  {
    const auto index = it.GetIndex();
    const auto inside = [](itk::IndexValueType i) {
      return i >= static_cast<itk::IndexValueType>(imageSize / 4) &&
             i < static_cast<itk::IndexValueType>(3 * imageSize / 4);
    };
    it.Set(inside(index[0]) && inside(index[1]) ? 1 : 0);
  }
  auto mask = itk::ImageMaskSpatialObject<Dimension>::New();
  mask->SetImage(maskImage);
  mask->Update();
  return mask;
}

class GlobalReleaseDataFlagGuard
{
public:
  GlobalReleaseDataFlagGuard()
    : m_Previous(itk::DataObject::GetGlobalReleaseDataFlag())
  {
    itk::DataObject::SetGlobalReleaseDataFlag(true);
  }
  ~GlobalReleaseDataFlagGuard() { itk::DataObject::SetGlobalReleaseDataFlag(m_Previous); }
  ITK_DISALLOW_COPY_AND_MOVE(GlobalReleaseDataFlagGuard);

private:
  bool m_Previous;
};

template <typename TImage>
void
AppendValues(const TImage * image, std::vector<double> & values)
{
  for (itk::ImageRegionConstIterator<TImage> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    for (unsigned int d = 0; d < Dimension; ++d)
    {
      values.push_back(it.Get()[d]);
    }
  }
}

// Runs a two-level registration, with the global release data flag on only during Update(), and returns the fields.
template <typename TRegistration>
std::vector<double>
Register(bool globalReleaseDataFlag, bool masked)
{
  using TransformType = typename TRegistration::OutputTransformType;

  const auto fixed = MakeFixedImage();
  const auto moving = MakeMovingImage(fixed);

  auto registration = TRegistration::New();
  auto transform = TransformType::New();

  typename TRegistration::ShrinkFactorsArrayType                   shrinkFactors(numberOfLevels);
  typename TRegistration::SmoothingSigmasArrayType                 sigmas(numberOfLevels);
  typename TRegistration::NumberOfIterationsArrayType              iterations(numberOfLevels);
  typename TRegistration::TransformParametersAdaptorsContainerType adaptors;
  for (unsigned int level = 0; level < numberOfLevels; ++level)
  {
    const unsigned int factor = 1u << (numberOfLevels - 1 - level);
    shrinkFactors[level] = factor;
    sigmas[level] = factor - 1;
    iterations[level] = 4;

    // The geometry of the fixed image shrunk by the factor, as ShrinkImageFilter computes it.
    const itk::SizeValueType levelSize = imageSize / factor;
    const double             levelSpacing = factor;
    const double             levelOrigin = 0.5 * (factor - 1);
    if constexpr (std::is_same_v<TRegistration,
                                 itk::TimeVaryingVelocityFieldImageRegistrationMethodv4<ImageType, ImageType>>)
    {
      using AdaptorType = itk::TimeVaryingVelocityFieldTransformParametersAdaptor<TransformType>;
      auto adaptor = AdaptorType::New();
      auto size = AdaptorType::SizeType::Filled(levelSize);
      size[Dimension] = 4;
      auto spacing = itk::MakeFilled<typename AdaptorType::SpacingType>(levelSpacing);
      spacing[Dimension] = 1.0;
      auto origin = itk::MakeFilled<typename AdaptorType::PointType>(levelOrigin);
      origin[Dimension] = 0.0;
      typename AdaptorType::DirectionType direction;
      direction.SetIdentity();
      adaptor->SetRequiredSize(size);
      adaptor->SetRequiredSpacing(spacing);
      adaptor->SetRequiredOrigin(origin);
      adaptor->SetRequiredDirection(direction);
      adaptors.push_back(adaptor);
    }
    else if constexpr (std::is_same_v<TRegistration, TimeVaryingBSplineRegistrationType>)
    {
      using AdaptorType = itk::TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TransformType>;
      auto adaptor = AdaptorType::New();
      auto size = AdaptorType::SizeType::Filled(levelSize);
      size[Dimension] = 5;
      auto spacing = itk::MakeFilled<typename AdaptorType::SpacingType>(levelSpacing);
      spacing[Dimension] = 0.25;
      auto origin = itk::MakeFilled<typename AdaptorType::OriginType>(levelOrigin);
      origin[Dimension] = 0.0;
      auto meshSize = itk::MakeFilled<typename AdaptorType::MeshSizeType>(2u << level);
      meshSize[Dimension] = 2;
      typename AdaptorType::DirectionType direction;
      direction.SetIdentity();
      adaptor->SetSplineOrder(transform->GetSplineOrder());
      adaptor->SetRequiredTransformDomainSize(size);
      adaptor->SetRequiredTransformDomainSpacing(spacing);
      adaptor->SetRequiredTransformDomainOrigin(origin);
      adaptor->SetRequiredTransformDomainMeshSize(meshSize);
      adaptor->SetRequiredTransformDomainDirection(direction);
      adaptors.push_back(adaptor);
    }
    else
    {
      using AdaptorType =
        std::conditional_t<std::is_same_v<TRegistration, itk::BSplineSyNImageRegistrationMethod<ImageType, ImageType>>,
                           itk::BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TransformType>,
                           itk::DisplacementFieldTransformParametersAdaptor<TransformType>>;
      auto adaptor = AdaptorType::New();
      adaptor->SetRequiredSize(AdaptorType::SizeType::Filled(levelSize));
      adaptor->SetRequiredSpacing(itk::MakeFilled<typename AdaptorType::SpacingType>(levelSpacing));
      adaptor->SetRequiredOrigin(itk::MakeFilled<typename AdaptorType::PointType>(levelOrigin));
      typename AdaptorType::DirectionType direction;
      direction.SetIdentity();
      adaptor->SetRequiredDirection(direction);
      adaptor->SetTransform(transform);
      if constexpr (std::is_same_v<TRegistration, itk::BSplineSyNImageRegistrationMethod<ImageType, ImageType>>)
      {
        adaptor->SetMeshSizeForTheUpdateField(itk::MakeFilled<typename TransformType::ArrayType>(4u << level));
        adaptor->SetMeshSizeForTheTotalField(itk::MakeFilled<typename TransformType::ArrayType>(0));
      }
      adaptors.push_back(adaptor);
    }
  }

  if constexpr (std::is_same_v<TRegistration,
                               itk::TimeVaryingVelocityFieldImageRegistrationMethodv4<ImageType, ImageType>>)
  {
    using VelocityFieldType = typename TransformType::VelocityFieldType;
    auto velocityField = VelocityFieldType::New();
    auto size = VelocityFieldType::SizeType::Filled(imageSize);
    size[Dimension] = 4;
    velocityField->SetRegions(size);
    velocityField->AllocateInitialized();
    transform->SetVelocityField(velocityField);
    transform->SetLowerTimeBound(0.0);
    transform->SetUpperTimeBound(1.0);
    transform->SetGaussianSpatialSmoothingVarianceForTheUpdateField(3.0);
    transform->SetGaussianTemporalSmoothingVarianceForTheUpdateField(0.5);
    transform->SetGaussianSpatialSmoothingVarianceForTheTotalField(0.0);
    transform->SetGaussianTemporalSmoothingVarianceForTheTotalField(0.0);
  }
  else if constexpr (std::is_same_v<TRegistration, TimeVaryingBSplineRegistrationType>)
  {
    // A zero control point lattice with the geometry that the first level's adaptor requires.
    using AdaptorType = itk::TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TransformType>;
    const auto * adaptor = static_cast<const AdaptorType *>(adaptors[0].GetPointer());
    auto         lattice = TransformType::TimeVaryingVelocityFieldControlPointLatticeType::New();
    lattice->SetRegions(adaptor->GetRequiredControlPointLatticeSize());
    lattice->SetOrigin(adaptor->GetRequiredControlPointLatticeOrigin());
    lattice->SetSpacing(adaptor->GetRequiredControlPointLatticeSpacing());
    lattice->SetDirection(adaptor->GetRequiredControlPointLatticeDirection());
    lattice->AllocateInitialized();
    transform->SetTimeVaryingVelocityFieldControlPointLattice(lattice);
    transform->SetVelocityFieldOrigin(adaptor->GetRequiredTransformDomainOrigin());
    transform->SetVelocityFieldSpacing(adaptor->GetRequiredTransformDomainSpacing());
    transform->SetVelocityFieldSize(adaptor->GetRequiredTransformDomainSize());
    transform->SetVelocityFieldDirection(adaptor->GetRequiredTransformDomainDirection());
    transform->SetLowerTimeBound(0.0);
    transform->SetUpperTimeBound(1.0);
    transform->IntegrateVelocityField();
  }
  else
  {
    auto field = FieldType::New();
    field->CopyInformation(fixed);
    field->SetRegions(fixed->GetBufferedRegion());
    field->AllocateInitialized();
    transform->SetDisplacementField(field);
    registration->SetGaussianSmoothingVarianceForTheTotalField(0.5);
    if constexpr (std::is_same_v<TRegistration, itk::SyNImageRegistrationMethod<ImageType, ImageType>>)
    {
      registration->SetGaussianSmoothingVarianceForTheUpdateField(3.0);
    }
  }

  registration->SetFixedImage(fixed);
  registration->SetMovingImage(moving);
  auto metric = MetricType::New();
  if (masked)
  {
    metric->SetFixedImageMask(MakeMask(fixed));
    metric->SetMovingImageMask(MakeMask(fixed));
  }
  registration->SetMetric(metric);
  registration->SetInitialTransform(transform);
  registration->InPlaceOn();
  registration->SetNumberOfLevels(numberOfLevels);
  registration->SetShrinkFactorsPerLevel(shrinkFactors);
  registration->SetSmoothingSigmasPerLevel(sigmas);
  registration->SetNumberOfIterationsPerLevel(iterations);
  registration->SetTransformParametersAdaptorsPerLevel(adaptors);
  registration->SetLearningRate(0.25);
  {
    std::optional<GlobalReleaseDataFlagGuard> releaseData;
    if (globalReleaseDataFlag)
    {
      releaseData.emplace();
    }
    registration->Update();
  }

  std::vector<double> values;
  AppendValues(transform->GetDisplacementField(), values);
  AppendValues(transform->GetInverseDisplacementField(), values);
  return values;
}

template <typename TRegistration>
void
CheckGlobalReleaseDataFlag(bool masked = false)
{
  const auto reference = Register<TRegistration>(false, masked);
  ASSERT_EQ(reference.size(), 2 * Dimension * imageSize * imageSize);
  ASSERT_TRUE(std::any_of(reference.begin(), reference.end(), [](double v) { return std::abs(v) > 1e-3; }));
  const auto values = Register<TRegistration>(true, masked);
  ASSERT_EQ(values.size(), reference.size());
  // Reports the first difference only, rather than both vectors.
  const auto mismatch = std::mismatch(values.begin(), values.end(), reference.begin());
  EXPECT_TRUE(mismatch.first == values.end()) << "first difference at " << (mismatch.first - values.begin()) << ": "
                                              << *mismatch.first << " != " << *mismatch.second;
}
} // namespace

TEST(GlobalReleaseDataFlag, SyNResultIsUnchanged)
{
  CheckGlobalReleaseDataFlag<itk::SyNImageRegistrationMethod<ImageType, ImageType>>();
}

TEST(GlobalReleaseDataFlag, BSplineSyNResultIsUnchanged)
{
  CheckGlobalReleaseDataFlag<itk::BSplineSyNImageRegistrationMethod<ImageType, ImageType>>();
}

TEST(GlobalReleaseDataFlag, MaskedSyNResultIsUnchanged)
{
  CheckGlobalReleaseDataFlag<itk::SyNImageRegistrationMethod<ImageType, ImageType>>(true);
}

TEST(GlobalReleaseDataFlag, MaskedBSplineSyNResultIsUnchanged)
{
  CheckGlobalReleaseDataFlag<itk::BSplineSyNImageRegistrationMethod<ImageType, ImageType>>(true);
}

TEST(GlobalReleaseDataFlag, TimeVaryingVelocityFieldResultIsUnchanged)
{
  CheckGlobalReleaseDataFlag<itk::TimeVaryingVelocityFieldImageRegistrationMethodv4<ImageType, ImageType>>();
}

namespace
{
using TwoChannelImageType = itk::Image<itk::Vector<double, 2>, Dimension>;
using ChannelAdaptorType = itk::NthElementImageAdaptor<TwoChannelImageType, double>;

// Channel 0 holds the given image, channel 1 the same image shifted by 3 pixels, so the two channels' gradients differ.
TwoChannelImageType::Pointer
MakeTwoChannelImage(const ImageType * image)
{
  auto twoChannel = TwoChannelImageType::New();
  twoChannel->CopyInformation(image);
  twoChannel->SetRegions(image->GetBufferedRegion());
  twoChannel->Allocate();
  for (itk::ImageRegionIteratorWithIndex<TwoChannelImageType> it(twoChannel, twoChannel->GetBufferedRegion());
       !it.IsAtEnd();
       ++it)
  {
    auto shifted = it.GetIndex();
    shifted[0] = (shifted[0] + 3) % static_cast<itk::IndexValueType>(imageSize);
    it.Set(itk::MakeVector(image->GetPixel(it.GetIndex()), image->GetPixel(shifted)));
  }
  return twoChannel;
}

ChannelAdaptorType::Pointer
MakeChannelAdaptor(TwoChannelImageType * image, unsigned int channel)
{
  auto adaptor = ChannelAdaptorType::New();
  adaptor->SetImage(image);
  adaptor->SelectNthElement(channel);
  return adaptor;
}
} // namespace

TEST(GlobalReleaseDataFlag, MetricKeepsTheAdaptorChannel)
{
  using AdaptorMetricType = itk::MeanSquaresImageToImageMetricv4<ChannelAdaptorType, ChannelAdaptorType, ImageType>;
  const auto fixed = MakeFixedImage();
  const auto fixedChannels = MakeTwoChannelImage(fixed);
  const auto movingChannels = MakeTwoChannelImage(MakeMovingImage(fixed));

  const auto valueAndDerivative = [&fixedChannels, &movingChannels] {
    auto transform = itk::TranslationTransform<double, Dimension>::New();
    transform->SetOffset(itk::MakeVector(0.5, -0.25));
    auto metric = AdaptorMetricType::New();
    metric->SetFixedImage(MakeChannelAdaptor(fixedChannels, 1));
    metric->SetMovingImage(MakeChannelAdaptor(movingChannels, 1));
    metric->SetMovingTransform(transform);
    metric->Initialize();
    AdaptorMetricType::MeasureType    value;
    AdaptorMetricType::DerivativeType derivative;
    metric->GetValueAndDerivative(value, derivative);
    return std::vector<double>{ value, derivative[0], derivative[1] };
  };
  const std::vector<double>        reference = valueAndDerivative();
  const GlobalReleaseDataFlagGuard releaseData;
  EXPECT_EQ(valueAndDerivative(), reference);
}

TEST(GlobalReleaseDataFlag, VelocityFieldIntegrationLeavesTheInterpolatorOnTheField)
{
  using TransformType = itk::TimeVaryingVelocityFieldTransform<double, Dimension>;
  using VelocityFieldType = TransformType::VelocityFieldType;
  auto velocityField = VelocityFieldType::New();
  velocityField->SetRegions(VelocityFieldType::SizeType{ { 8, 8, 4 } });
  velocityField->Allocate();
  velocityField->FillBuffer(itk::MakeVector(1.0, -2.0));

  auto transform = TransformType::New();
  transform->SetVelocityField(velocityField);
  {
    const GlobalReleaseDataFlagGuard releaseData;
    transform->IntegrateVelocityField();
  }
  EXPECT_EQ(transform->GetVelocityFieldInterpolator()->GetInputImage(), velocityField.GetPointer());
  const TransformType::VelocityFieldInterpolatorType::PointType point{ { 3.0, 4.0, 1.0 } };
  EXPECT_EQ(transform->GetVelocityFieldInterpolator()->Evaluate(point), velocityField->GetPixel({ { 3, 4, 1 } }));
}

TEST(GlobalReleaseDataFlag, TimeVaryingBSplineVelocityFieldResultIsUnchanged)
{
  CheckGlobalReleaseDataFlag<TimeVaryingBSplineRegistrationType>();
}

TEST(GlobalReleaseDataFlag, MaskedTimeVaryingBSplineVelocityFieldResultIsUnchanged)
{
  CheckGlobalReleaseDataFlag<TimeVaryingBSplineRegistrationType>(true);
}
