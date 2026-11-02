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

#include "itkSyNImageRegistrationMethod.h"
#include "itkDisplacementFieldTransformParametersAdaptor.h"
#include "itkImageMaskSpatialObject.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkObjectToObjectMultiMetricv4.h"
#include "itkResampleImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkGTest.h"

#include <cmath>
#include <algorithm>
#include <vector>

namespace
{
constexpr unsigned int Dimension = 2;
using ImageType = itk::Image<double, Dimension>;
using TransformType = itk::DisplacementFieldTransform<double, Dimension>;
using FieldType = TransformType::DisplacementFieldType;
using RegistrationType = itk::SyNImageRegistrationMethod<ImageType, ImageType, TransformType>;
using MetricType = itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType>;
using MultiMetricType = itk::ObjectToObjectMultiMetricv4<Dimension, Dimension, ImageType>;
using MaskImageType = itk::Image<unsigned char, Dimension>;

constexpr itk::SizeValueType imageSize = 48;

struct Options
{
  double updateVariance{ 3.0 };
  double totalVariance{ 0.0 };
  bool   downsample{ true };
  bool   mask{ false };
  bool   multi{ false };
};

ImageType::Pointer
MakeFixedImage()
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(imageSize));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const double x = it.GetIndex()[0] - 24.0;
    const double y = it.GetIndex()[1] - 22.0;
    it.Set(100.0 * std::exp(-(x * x + 2.0 * y * y) / 120.0) + 40.0 * std::exp(-((x - 10) * (x - 10) + y * y) / 30.0));
  }
  return image;
}

ImageType::Pointer
Warp(const ImageType * image)
{
  auto warp = FieldType::New();
  warp->CopyInformation(image);
  warp->SetRegions(image->GetBufferedRegion());
  warp->Allocate();
  for (itk::ImageRegionIteratorWithIndex<FieldType> it(warp, warp->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    FieldType::PixelType v;
    v[0] = 2.0 * std::sin(2.0 * itk::Math::pi * it.GetIndex()[1] / imageSize);
    v[1] = 1.5 * std::cos(2.0 * itk::Math::pi * it.GetIndex()[0] / imageSize);
    it.Set(v);
  }
  auto warpTransform = TransformType::New();
  warpTransform->SetDisplacementField(warp);
  auto resampler = itk::ResampleImageFilter<ImageType, ImageType, double, double>::New();
  resampler->SetInput(image);
  resampler->SetTransform(warpTransform);
  resampler->SetReferenceImage(image);
  resampler->UseReferenceImageOn();
  resampler->Update();
  ImageType::Pointer warped = resampler->GetOutput();
  warped->DisconnectPipeline();
  return warped;
}

MetricType::Pointer
MakeMetric()
{
  auto metric = MetricType::New();
  metric->SetUseMovingImageGradientFilter(false);
  metric->SetUseFixedImageGradientFilter(false);
  return metric;
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

// Runs a two-level SyN and returns the output displacement and inverse displacement fields.
std::vector<double>
RunSyN(const Options & options)
{
  const auto fixed = MakeFixedImage();
  const auto moving = Warp(fixed);

  auto metric = MakeMetric();
  if (options.mask)
  {
    auto maskImage = MaskImageType::New();
    maskImage->CopyInformation(fixed);
    maskImage->SetRegions(fixed->GetBufferedRegion());
    maskImage->Allocate();
    for (itk::ImageRegionIteratorWithIndex<MaskImageType> it(maskImage, maskImage->GetBufferedRegion()); !it.IsAtEnd();
         ++it)
    {
      it.Set(fixed->GetPixel(it.GetIndex()) > 5.0 ? 1 : 0);
    }
    auto mask = itk::ImageMaskSpatialObject<Dimension>::New();
    mask->SetImage(maskImage);
    mask->Update();
    metric->SetFixedImageMask(mask);
  }

  auto field = FieldType::New();
  field->CopyInformation(fixed);
  field->SetRegions(fixed->GetBufferedRegion());
  field->AllocateInitialized();
  auto inverse = FieldType::New();
  inverse->CopyInformation(fixed);
  inverse->SetRegions(fixed->GetBufferedRegion());
  inverse->AllocateInitialized();
  auto outputTransform = TransformType::New();
  outputTransform->SetDisplacementField(field);
  outputTransform->SetInverseDisplacementField(inverse);

  constexpr unsigned int                                     numberOfLevels = 2;
  RegistrationType::ShrinkFactorsArrayType                   shrinkFactors(numberOfLevels);
  RegistrationType::SmoothingSigmasArrayType                 sigmas(numberOfLevels);
  RegistrationType::NumberOfIterationsArrayType              iterations(numberOfLevels);
  RegistrationType::TransformParametersAdaptorsContainerType adaptors;
  for (unsigned int level = 0; level < numberOfLevels; ++level)
  {
    const unsigned int factor = 1u << (numberOfLevels - 1 - level);
    shrinkFactors[level] = factor;
    sigmas[level] = factor - 1;
    iterations[level] = 8;
    auto fieldShrinker = itk::ShrinkImageFilter<FieldType, FieldType>::New();
    fieldShrinker->SetShrinkFactors(factor);
    fieldShrinker->SetInput(field);
    fieldShrinker->Update();
    auto adaptor = itk::DisplacementFieldTransformParametersAdaptor<TransformType>::New();
    adaptor->SetRequiredSpacing(fieldShrinker->GetOutput()->GetSpacing());
    adaptor->SetRequiredSize(fieldShrinker->GetOutput()->GetBufferedRegion().GetSize());
    adaptor->SetRequiredDirection(fieldShrinker->GetOutput()->GetDirection());
    adaptor->SetRequiredOrigin(fieldShrinker->GetOutput()->GetOrigin());
    adaptor->SetTransform(outputTransform);
    adaptors.push_back(adaptor);
  }

  auto registration = RegistrationType::New();
  registration->SetFixedImage(fixed);
  registration->SetMovingImage(moving);
  if (options.multi)
  {
    // A second image pair, with its own intensities, weighted equally with the first.
    auto multi = MultiMetricType::New();
    multi->AddMetric(metric);
    multi->AddMetric(MakeMetric());
    registration->SetFixedImage(1, Warp(Warp(fixed)));
    registration->SetMovingImage(1, Warp(fixed));
    registration->SetMetric(multi);
  }
  else
  {
    registration->SetMetric(metric);
  }
  registration->SetInitialTransform(outputTransform);
  registration->InPlaceOn();
  registration->SetNumberOfLevels(numberOfLevels);
  registration->SetShrinkFactorsPerLevel(shrinkFactors);
  registration->SetSmoothingSigmasPerLevel(sigmas);
  registration->SetSmoothingSigmasAreSpecifiedInPhysicalUnits(false);
  registration->SetNumberOfIterationsPerLevel(iterations);
  registration->SetTransformParametersAdaptorsPerLevel(adaptors);
  registration->SetLearningRate(0.25);
  registration->SetGaussianSmoothingVarianceForTheUpdateField(options.updateVariance);
  registration->SetGaussianSmoothingVarianceForTheTotalField(options.totalVariance);
  registration->SetConvergenceThreshold(1e-12);
  registration->SetConvergenceWindowSize(10);
  registration->SetDownsampleImagesForMetricDerivatives(options.downsample);
  registration->SetAverageMidPointGradients(false);
  registration->Update();

  std::vector<double> values;
  for (const FieldType * result :
       { outputTransform->GetDisplacementField(), outputTransform->GetInverseDisplacementField() })
  {
    for (itk::ImageRegionConstIterator<FieldType> it(result, result->GetBufferedRegion()); !it.IsAtEnd(); ++it)
    {
      values.push_back(it.Get()[0]);
      values.push_back(it.Get()[1]);
    }
  }
  return values;
}

const std::vector<Options> &
Configurations()
{
  // Default, smoothed total field with an update variance below 0.5, mask without derivative downsampling,
  // and two image pairs.
  static const std::vector<Options> configurations{
    {}, { 0.25, 1.0, true, false, false }, { 3.0, 0.0, false, true, false }, { 3.0, 0.5, true, false, true }
  };
  return configurations;
}
} // namespace

TEST(SyNImageRegistrationMethod, MatchesBaseline)
{
  const std::vector<itk::SizeValueType> offsets{ 2 * (12 * imageSize + 30),
                                                 2 * (24 * imageSize + 24) + 1,
                                                 2 * (30 * imageSize + 15),
                                                 2 * imageSize * imageSize + 2 * (20 * imageSize + 20) };
  // Values from ITK main.
  const std::vector<std::vector<double>> baseline{
    { -0.28336780395289196, 1.274287937792419, 0.21522601031570865, 0.11025017536445247 },
    { -0.27311490551753081, 1.3763187723850656, 0.20881543269035774, 0.19702995233577525 },
    { -0.27268737095291229, 1.3008609297425906, 0.21090716804163279, 0.17900305205919409 },
    { -0.34488616123441318, 0.23092394821163253, 0.11462549081682383, -0.63336005356165614 }
  };
  for (size_t c = 0; c < Configurations().size(); ++c)
  {
    const auto values = RunSyN(Configurations()[c]);
    double     scale = 0.0;
    for (const double v : values)
    {
      scale = std::max(scale, std::abs(v));
    }
    ASSERT_GT(scale, 0.1) << "configuration " << c;
    for (size_t k = 0; k < offsets.size(); ++k)
    {
      EXPECT_NEAR(values[offsets[k]], baseline[c][k], 1e-6 * scale)
        << "configuration " << c << ", offset " << offsets[k];
    }
  }
}

namespace
{
// The pixels of a field, in buffer order.
std::vector<FieldType::PixelType>
Pixels(const FieldType * field)
{
  std::vector<FieldType::PixelType> pixels;
  for (itk::ImageRegionConstIterator<FieldType> it(field, field->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    pixels.push_back(it.Get());
  }
  return pixels;
}
} // namespace

// Exposes the protected smoothing function.
class SmoothingSyN : public RegistrationType
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SmoothingSyN);
  using Self = SmoothingSyN;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  using RegistrationType::GaussianSmoothDisplacementField;

protected:
  SmoothingSyN() = default;
};

TEST(SyNImageRegistrationMethod, GaussianSmoothingKeepsTheInputFieldWithGlobalReleaseDataFlag)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::SizeType::Filled(20));
  field->Allocate();
  for (itk::ImageRegionIteratorWithIndex<FieldType> it(field, field->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    FieldType::PixelType v;
    v[0] = std::sin(0.5 * it.GetIndex()[0]) + 0.1 * it.GetIndex()[1];
    v[1] = std::cos(0.3 * it.GetIndex()[1]);
    it.Set(v);
  }
  const std::vector<FieldType::PixelType> input = Pixels(field);

  auto       syn = SmoothingSyN::New();
  const auto smooth = [&syn, &field](double variance) {
    return Pixels(syn->GaussianSmoothDisplacementField(field, variance));
  };
  for (const double variance : { 0.0, 0.25, 2.0 })
  {
    const auto reference = smooth(variance);
    ASSERT_EQ(reference.size(), input.size());
    {
      const GlobalReleaseDataFlagGuard releaseData;
      EXPECT_EQ(smooth(variance), reference) << "variance " << variance;
    }
    ASSERT_EQ(field->GetBufferedRegion(), field->GetLargestPossibleRegion()) << "variance " << variance;
    EXPECT_EQ(Pixels(field), input) << "variance " << variance;
  }
}
