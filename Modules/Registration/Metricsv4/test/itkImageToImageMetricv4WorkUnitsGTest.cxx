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

#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkGTest.h"
#include "itkImageRegionIterator.h"
#include "itkTranslationTransform.h"

namespace
{
using ImageType = itk::Image<float, 3>;
using MetricType = itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType>;

ImageType::Pointer
MakeImage()
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(24));
  image->AllocateInitialized();
  float value = 0;
  for (itk::ImageRegionIterator<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    it.Set(value += 0.5F);
  }
  return image;
}

MetricType::Pointer
MakeMetric(const ImageType * image, const bool sparse)
{
  auto metric = MetricType::New();
  metric->SetFixedImage(image);
  metric->SetMovingImage(image);
  metric->SetMovingTransform(itk::TranslationTransform<double, 3>::New());
  if (!sparse)
  {
    return metric;
  }

  auto points = MetricType::FixedSampledPointSetType::New();
  for (itk::IdentifierType id = 0; id < 4000; ++id)
  {
    MetricType::FixedSampledPointSetType::PointType point;
    point.Fill(1.0 + static_cast<double>(id % 20));
    points->SetPoint(id, point);
  }
  metric->SetFixedSampledPointSet(points);
  metric->SetUseSampledPointSet(true);
  return metric;
}

itk::ThreadIdType
WorkUnitsUsed(MetricType & metric)
{
  metric.Initialize();
  MetricType::MeasureType    value{};
  MetricType::DerivativeType derivative;
  metric.GetValueAndDerivative(value, derivative);
  return metric.GetNumberOfWorkUnitsUsed();
}

std::string
SparseOrDenseName(const ::testing::TestParamInfo<bool> & paramInfo)
{
  return paramInfo.param ? "Sparse" : "Dense";
}
} // namespace

class ImageToImageMetricv4WorkUnits : public ::testing::TestWithParam<bool>
{};

TEST_P(ImageToImageMetricv4WorkUnits, SetMaximumNumberOfWorkUnitsEqualToThreadCount)
{
  const auto image = MakeImage();
  const auto metric = MakeMetric(image, GetParam());

  const itk::ThreadIdType threads = metric->GetMaximumNumberOfWorkUnits();
  ASSERT_GE(threads, 1u);
  metric->SetMaximumNumberOfWorkUnits(threads);
  EXPECT_LE(WorkUnitsUsed(*metric), threads);
}

TEST_P(ImageToImageMetricv4WorkUnits, SetMaximumNumberOfWorkUnitsToOne)
{
  const auto image = MakeImage();
  const auto metric = MakeMetric(image, GetParam());

  metric->SetMaximumNumberOfWorkUnits(1);
  EXPECT_EQ(WorkUnitsUsed(*metric), 1u);
}

INSTANTIATE_TEST_SUITE_P(SparseAndDense, ImageToImageMetricv4WorkUnits, ::testing::Bool(), SparseOrDenseName);
