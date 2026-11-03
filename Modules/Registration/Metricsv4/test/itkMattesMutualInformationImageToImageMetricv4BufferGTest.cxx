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

#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkBSplineTransform.h"
#include "itkGTest.h"
#include "itkImageRegionIteratorWithIndex.h"

#include <cmath>
#include <cstdint>
#include <limits>

namespace
{
using ImageType = itk::Image<float, 3>;
using MattesType = itk::MattesMutualInformationImageToImageMetricv4<ImageType, ImageType>;
using BSplineType = itk::BSplineTransform<double, 3, 3>;

class BufferTestMetric : public MattesType
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BufferTestMetric);
  using Self = BufferTestMetric;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  using MattesType::ComputeDerivativeBufferBytesPerWorkUnit;

protected:
  BufferTestMetric() = default;
};

constexpr std::uint64_t OneMiB = std::uint64_t{ 1 } << 20;
constexpr std::uint64_t OneGiB = std::uint64_t{ 1 } << 30;

ImageType::Pointer
MakeBlob(double shift)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(24));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    double r2 = 0;
    for (unsigned int d = 0; d < 3; ++d)
    {
      const double x = it.GetIndex()[d] - 11.5 - (d == 0 ? shift : 0.0);
      r2 += x * x;
    }
    it.Set(static_cast<float>(100.0 * std::exp(-r2 / 40.0)));
  }
  return image;
}

BufferTestMetric::Pointer
MakeMetric(const ImageType * fixed, const ImageType * moving)
{
  auto bspline = BSplineType::New();
  bspline->SetTransformDomainOrigin(fixed->GetOrigin());
  BSplineType::PhysicalDimensionsType dimensions;
  dimensions.Fill(23.0);
  bspline->SetTransformDomainPhysicalDimensions(dimensions);
  bspline->SetTransformDomainMeshSize(BSplineType::MeshSizeType::Filled(4));
  bspline->SetTransformDomainDirection(fixed->GetDirection());
  auto parameters = bspline->GetParameters();
  parameters.Fill(0.05);
  bspline->SetParameters(parameters);

  auto metric = BufferTestMetric::New();
  metric->SetFixedImage(fixed);
  metric->SetMovingImage(moving);
  metric->SetMovingTransform(bspline);
  metric->SetNumberOfHistogramBins(20);
  metric->Initialize();
  return metric;
}
} // namespace

TEST(MattesMutualInformationImageToImageMetricv4Buffer, DefaultIsAutomatic)
{
  const auto metric = MattesType::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(metric, MattesMutualInformationImageToImageMetricv4, ImageToImageMetricv4);
  EXPECT_EQ(metric->GetMaximumDerivativeBufferBytesPerWorkUnit(), 0u);
}

TEST(MattesMutualInformationImageToImageMetricv4Buffer, ExplicitValueIsUsed)
{
  const auto fixed = MakeBlob(0.0);
  const auto metric = MakeMetric(fixed, fixed);
  metric->SetMaximumDerivativeBufferBytesPerWorkUnit(123456);
  EXPECT_EQ(metric->GetMaximumDerivativeBufferBytesPerWorkUnit(), 123456u);
  EXPECT_EQ(metric->ComputeDerivativeBufferBytesPerWorkUnit(8), 123456u);
}

TEST(MattesMutualInformationImageToImageMetricv4Buffer, AutomaticStaysWithinBounds)
{
  const auto fixed = MakeBlob(0.0);
  const auto metric = MakeMetric(fixed, fixed);
  for (const itk::ThreadIdType workUnits : { 1u, 8u, 48u, 768u })
  {
    const auto bytes = metric->ComputeDerivativeBufferBytesPerWorkUnit(workUnits);
    EXPECT_GE(bytes, 32 * OneMiB) << workUnits;
    EXPECT_LE(bytes, 1024 * OneMiB) << workUnits;
  }
}

TEST(MattesMutualInformationImageToImageMetricv4Buffer, AutomaticUsesFloorWhenSamplesFitInIt)
{
  const auto fixed = MakeBlob(0.0);
  const auto metric = MakeMetric(fixed, fixed);
  // 24^3 samples over 16 work units fill about 27 MiB of 1,029-parameter rows.
  EXPECT_EQ(metric->ComputeDerivativeBufferBytesPerWorkUnit(16), 32 * OneMiB);
}

TEST(MattesMutualInformationImageToImageMetricv4Buffer, DerivativeDoesNotDependOnBufferSize)
{
  const auto fixed = MakeBlob(0.0);
  const auto moving = MakeBlob(1.5);

  const auto smallBuffer = MakeMetric(fixed, moving);
  smallBuffer->SetMaximumDerivativeBufferBytesPerWorkUnit(1);
  MattesType::MeasureType    smallValue{};
  MattesType::DerivativeType smallDerivative;
  smallBuffer->GetValueAndDerivative(smallValue, smallDerivative);

  const auto largeBuffer = MakeMetric(fixed, moving);
  largeBuffer->SetMaximumDerivativeBufferBytesPerWorkUnit(1024 * OneMiB);
  MattesType::MeasureType    largeValue{};
  MattesType::DerivativeType largeDerivative;
  largeBuffer->GetValueAndDerivative(largeValue, largeDerivative);

  EXPECT_EQ(smallValue, largeValue);
  ASSERT_EQ(smallDerivative.Size(), largeDerivative.Size());
  double maximumMagnitude = 0;
  for (unsigned int i = 0; i < largeDerivative.Size(); ++i)
  {
    maximumMagnitude = std::max(maximumMagnitude, std::abs(largeDerivative[i]));
  }
  ASSERT_GT(maximumMagnitude, 0.0);
  for (unsigned int i = 0; i < largeDerivative.Size(); ++i)
  {
    EXPECT_NEAR(smallDerivative[i], largeDerivative[i], 1e-10 * maximumMagnitude) << i;
  }
}

TEST(MattesMutualInformationImageToImageMetricv4Buffer, DefaultFollowsGivenPhysicalMemory)
{
  constexpr itk::SizeValueType samples = itk::SizeValueType{ 512 } * 512 * 512;
  constexpr itk::SizeValueType parameters = 3993;
  constexpr itk::SizeValueType workUnits = 512;
  constexpr std::uint64_t      GiB = OneGiB;

  // As if the host had 64 GiB: a quarter of it over 512 work units is the 32 MiB floor.
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(samples, parameters, workUnits, 64 * GiB),
            32 * OneMiB);
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(samples, parameters, workUnits, 256 * GiB),
            128 * OneMiB);
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(samples, parameters, workUnits, 64 * GiB, 0.5),
            64 * OneMiB);
}

TEST(MattesMutualInformationImageToImageMetricv4Buffer, DefaultStaysBetweenFloorAndCap)
{
  constexpr std::uint64_t GiB = OneGiB;
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(24 * 24 * 24, 1029, 16, 64 * GiB), 32 * OneMiB);
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(1000000, 3993, 1, 1024 * GiB), GiB);
  // Four rows per sample: 2,000 samples on one work unit fill 2001 * 4 rows of 3,993 doubles.
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(2000, 3993, 1, 1024 * GiB),
            std::uint64_t{ 2001 } * 4 * 8 * 3993);
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(2000, 3993, 1, 1024 * GiB, std::nan("")),
            32 * OneMiB);
  constexpr auto largest = std::numeric_limits<itk::SizeValueType>::max();
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(largest, largest, 1, 1024 * GiB), GiB);
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(
              2000000, 3993, 1, std::numeric_limits<std::uint64_t>::max(), 1.0),
            GiB);
}

TEST(MattesMutualInformationImageToImageMetricv4Buffer, ZeroPhysicalMemoryUsesHost)
{
  const auto host = MattesType::GetHostPhysicalMemoryBytes();
  if (host == 0)
  {
    GTEST_SKIP() << "Physical memory is not readable on this platform.";
  }
  constexpr itk::SizeValueType samples = itk::SizeValueType{ 256 } * 256 * 256;
  EXPECT_EQ(MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(samples, 3993, 96),
            MattesType::ComputeDefaultDerivativeBufferBytesPerWorkUnit(samples, 3993, 96, host));
}
