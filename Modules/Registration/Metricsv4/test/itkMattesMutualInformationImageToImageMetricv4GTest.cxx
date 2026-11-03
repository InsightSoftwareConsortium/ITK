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
#include "itkAffineTransform.h"
#include "itkBSplineTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMultiThreaderBase.h"
#include "itkTranslationTransform.h"
#include "itkGTest.h"

#include <algorithm>
#include <cmath>
#include <vector>

namespace
{
constexpr unsigned int Dimension = 3;
using ImageType = itk::Image<double, Dimension>;
using MetricType = itk::MattesMutualInformationImageToImageMetricv4<ImageType, ImageType>;
using DisplacementTransformType = itk::DisplacementFieldTransform<double, Dimension>;

// A translation whose Jacobian is nonzero only at one point, so that its global derivative is the
// contribution of that point alone, which is what a local-support transform reports for that point.
class SinglePointTranslationTransform : public itk::TranslationTransform<double, Dimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SinglePointTranslationTransform);

  using Self = SinglePointTranslationTransform;
  using Superclass = itk::TranslationTransform<double, Dimension>;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(SinglePointTranslationTransform);

  void
  ComputeJacobianWithRespectToParameters(const InputPointType & point, JacobianType & jacobian) const override
  {
    Superclass::ComputeJacobianWithRespectToParameters(point, jacobian);
    if (point.EuclideanDistanceTo(m_Target) > 1e-6)
    {
      jacobian.fill(0.0);
    }
  }

  InputPointType m_Target{};

protected:
  SinglePointTranslationTransform() = default;
};

ImageType::Pointer
MakeImage(double shift)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(16));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const auto & index = it.GetIndex();
    const double x = index[0] + shift;
    it.Set(100.0 + 50.0 * std::sin(0.4 * x) * std::cos(0.3 * index[1]) + 2.0 * index[2]);
  }
  return image;
}

std::vector<double>
ComputeDerivative(const ImageType *                            fixed,
                  const ImageType *                            moving,
                  MetricType::MovingTransformType *            transform,
                  const MetricType::FixedSampledPointSetType * points)
{
  auto metric = MetricType::New();
  metric->SetNumberOfHistogramBins(20);
  metric->SetFixedImage(fixed);
  metric->SetMovingImage(moving);
  metric->SetMovingTransform(transform);
  if (points != nullptr)
  {
    metric->SetFixedSampledPointSet(points);
    metric->SetUseSampledPointSet(true);
  }
  metric->Initialize();

  MetricType::MeasureType    value;
  MetricType::DerivativeType derivative;
  metric->GetValueAndDerivative(value, derivative);
  return { derivative.begin(), derivative.end() };
}

void
CheckLocalSupportDerivative(bool sparse)
{
  const auto fixed = MakeImage(0.0);
  const auto moving = MakeImage(0.7);

  auto field = DisplacementTransformType::DisplacementFieldType::New();
  field->CopyInformation(fixed);
  field->SetRegions(fixed->GetLargestPossibleRegion());
  field->Allocate();
  field->FillBuffer(DisplacementTransformType::DisplacementFieldType::PixelType{});
  auto displacementTransform = DisplacementTransformType::New();
  displacementTransform->SetDisplacementField(field);

  // Sparse sampling uses every second voxel in each dimension.
  MetricType::FixedSampledPointSetType::Pointer points;
  std::vector<bool>                             sampled(fixed->GetLargestPossibleRegion().GetNumberOfPixels(), !sparse);
  if (sparse)
  {
    points = MetricType::FixedSampledPointSetType::New();
    itk::SizeValueType count = 0;
    for (itk::ImageRegionIteratorWithIndex<ImageType> it(fixed, fixed->GetBufferedRegion()); !it.IsAtEnd(); ++it)
    {
      const auto & index = it.GetIndex();
      if (index[0] % 2 == 0 && index[1] % 2 == 0 && index[2] % 2 == 0)
      {
        ImageType::PointType point;
        fixed->TransformIndexToPhysicalPoint(index, point);
        points->SetPoint(count++, point);
        sampled[fixed->ComputeOffset(index)] = true;
      }
    }
  }

  const auto local = ComputeDerivative(fixed, moving, displacementTransform, points);
  ASSERT_EQ(local.size(), sampled.size() * Dimension);
  const double scale = std::abs(
    *std::max_element(local.begin(), local.end(), [](double a, double b) { return std::abs(a) < std::abs(b); }));
  ASSERT_GT(scale, 1e-6);
  const double tolerance = 1e-10 * scale;
  EXPECT_EQ(ComputeDerivative(fixed, moving, displacementTransform, points), local);

  for (size_t voxel = 0; voxel < sampled.size(); ++voxel)
  {
    if (!sampled[voxel])
    {
      for (unsigned int d = 0; d < Dimension; ++d)
      {
        EXPECT_EQ(local[voxel * Dimension + d], 0.0) << "unsampled voxel " << voxel;
      }
    }
  }

  // The sum over points equals the derivative of a translation, which every point moves.
  const auto translation =
    ComputeDerivative(fixed, moving, itk::TranslationTransform<double, Dimension>::New(), points);
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    double sum = 0.0;
    for (size_t voxel = 0; voxel < sampled.size(); ++voxel)
    {
      sum += local[voxel * Dimension + d];
    }
    EXPECT_NEAR(sum, translation[d], 1e-9 * std::max(scale, std::abs(translation[d]))) << "dimension " << d;
  }

  // Each point's derivative equals that of a translation that moves only this point.
  const std::vector<ImageType::IndexType> checked{
    { { 0, 0, 0 } }, { { 4, 6, 2 } }, { { 8, 2, 10 } }, { { 14, 14, 14 } }
  };
  for (const auto & index : checked)
  {
    auto singlePoint = SinglePointTranslationTransform::New();
    fixed->TransformIndexToPhysicalPoint(index, singlePoint->m_Target);
    const auto   reference = ComputeDerivative(fixed, moving, singlePoint, points);
    const size_t voxel = fixed->ComputeOffset(index);
    ASSERT_TRUE(sampled[voxel]);
    for (unsigned int d = 0; d < Dimension; ++d)
    {
      EXPECT_NEAR(local[voxel * Dimension + d], reference[d], tolerance) << "index " << index << " dimension " << d;
    }
  }
}
} // namespace

TEST(MattesMutualInformationImageToImageMetricv4, DenseLocalSupportDerivativeMatchesPerPointReference)
{
  CheckLocalSupportDerivative(false);
}

TEST(MattesMutualInformationImageToImageMetricv4, SparseLocalSupportDerivativeMatchesPerPointReference)
{
  CheckLocalSupportDerivative(true);
}

namespace
{
ImageType::Pointer
MakeReproducibilityImage()
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(48));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const auto & index = it.GetIndex();
    it.Set(100.0 + 50.0 * std::sin(0.2 * index[0]) * std::cos(0.15 * index[1]) + 0.5 * index[2]);
  }
  return image;
}

MetricType::MovingTransformType::Pointer
MakeAffine()
{
  auto transform = itk::AffineTransform<double, Dimension>::New();
  auto parameters = transform->GetParameters();
  parameters[0] = 1.02;
  parameters[4] = 0.98;
  parameters[9] = 1.5;
  parameters[10] = -2.0;
  transform->SetParameters(parameters);
  return transform;
}

MetricType::MovingTransformType::Pointer
MakeBSpline(const ImageType * image)
{
  using TransformType = itk::BSplineTransform<double, Dimension, 3>;
  auto                                  transform = TransformType::New();
  TransformType::PhysicalDimensionsType dimensions;
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    dimensions[d] = image->GetSpacing()[d] * (image->GetLargestPossibleRegion().GetSize(d) - 1);
  }
  transform->SetTransformDomainOrigin(image->GetOrigin());
  transform->SetTransformDomainPhysicalDimensions(dimensions);
  transform->SetTransformDomainMeshSize(TransformType::MeshSizeType::Filled(3));
  transform->SetTransformDomainDirection(image->GetDirection());
  auto parameters = transform->GetParameters();
  for (unsigned int i = 0; i < parameters.Size(); ++i)
  {
    parameters[i] = 0.5 * std::sin(0.1 * i);
  }
  transform->SetParameters(parameters);
  return transform;
}

// The derivative with the value appended, and the work units used, of one evaluation.
struct Result
{
  std::vector<double> valueAndDerivative;
  itk::ThreadIdType   numberOfWorkUnitsUsed;
};

Result
Evaluate(MetricType * metric)
{
  metric->Initialize();
  MetricType::MeasureType    value;
  MetricType::DerivativeType derivative;
  metric->GetValueAndDerivative(value, derivative);
  Result result{ { derivative.begin(), derivative.end() }, metric->GetNumberOfWorkUnitsUsed() };
  result.valueAndDerivative.push_back(value);
  return result;
}

// A metric whose threader can run several work units: SingleMultiThreader, if it is the default, always runs one.
MetricType::Pointer
NewMetric()
{
  using ThreaderEnum = itk::MultiThreaderBase::ThreaderEnum;
  const auto previousThreader = itk::MultiThreaderBase::GetGlobalDefaultThreader();
  if (previousThreader == ThreaderEnum::Single)
  {
    itk::MultiThreaderBase::SetGlobalDefaultThreader(ThreaderEnum::Pool);
  }
  auto metric = MetricType::New();
  itk::MultiThreaderBase::SetGlobalDefaultThreader(previousThreader);
  return metric;
}

// A dense metric with 32 bins of the image against itself.
MetricType::Pointer
MakeMetric(const ImageType * image, MetricType::MovingTransformType * transform, itk::ThreadIdType numberOfWorkUnits)
{
  auto metric = NewMetric();
  metric->SetNumberOfHistogramBins(32);
  metric->SetFixedImage(image);
  metric->SetMovingImage(image);
  metric->SetMovingTransform(transform);
  metric->SetMaximumNumberOfWorkUnits(numberOfWorkUnits);
  return metric;
}

Result
ComputeValueAndDerivative(const ImageType *                 image,
                          MetricType::MovingTransformType * transform,
                          itk::ThreadIdType                 numberOfWorkUnits)
{
  return Evaluate(MakeMetric(image, transform, numberOfWorkUnits));
}

void
CheckReproducible(const ImageType * image, MetricType::MovingTransformType * transform)
{
  // No more work units than the build allows; the tests skip below 2.
  const itk::ThreadIdType numberOfWorkUnits = std::min<itk::ThreadIdType>(16, itk::ITK_MAX_THREADS);

  // A single work unit gives the reference values up to the summation order.
  const auto serial = ComputeValueAndDerivative(image, transform, 1).valueAndDerivative;
  ASSERT_TRUE(std::any_of(serial.begin(), serial.end() - 1, [](double d) { return std::abs(d) > 1e-3; }));

  const auto reference = ComputeValueAndDerivative(image, transform, numberOfWorkUnits);
  ASSERT_EQ(reference.numberOfWorkUnitsUsed, numberOfWorkUnits);
  ASSERT_EQ(reference.valueAndDerivative.size(), serial.size());
  for (size_t i = 0; i < serial.size(); ++i)
  {
    EXPECT_NEAR(reference.valueAndDerivative[i], serial[i], 1e-9 * std::max(1.0, std::abs(serial[i]))) << i;
  }

  for (int repeat = 0; repeat < 10; ++repeat)
  {
    const auto result = ComputeValueAndDerivative(image, transform, numberOfWorkUnits);
    EXPECT_EQ(result.numberOfWorkUnitsUsed, numberOfWorkUnits) << "repeat " << repeat;
    EXPECT_EQ(result.valueAndDerivative, reference.valueAndDerivative) << "repeat " << repeat;
  }
}
} // namespace

TEST(MattesMutualInformationImageToImageMetricv4, AffineDerivativeIsReproducible)
{
  if (itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads() < 2 || itk::ITK_MAX_THREADS < 2)
  {
    GTEST_SKIP() << "Needs at least 2 threads to run work units concurrently.";
  }
  const auto image = MakeReproducibilityImage();
  CheckReproducible(image, MakeAffine());
}

TEST(MattesMutualInformationImageToImageMetricv4, BSplineDerivativeIsReproducible)
{
  if (itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads() < 2 || itk::ITK_MAX_THREADS < 2)
  {
    GTEST_SKIP() << "Needs at least 2 threads to run work units concurrently.";
  }
  const auto image = MakeReproducibilityImage();
  CheckReproducible(image, MakeBSpline(image));
}
