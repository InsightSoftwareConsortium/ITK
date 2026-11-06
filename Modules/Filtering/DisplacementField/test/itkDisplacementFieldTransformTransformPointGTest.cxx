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

#include "itkDisplacementFieldTransform.h"
#include "itkEuler3DTransform.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction.h"
#include "itkGTest.h"

#include <algorithm>
#include <cmath>
#include <random>

namespace
{
// Clamps the index so a bounds check made on another image's geometry cannot read outside this buffer.
template <typename TImage, typename TScalar>
class ClampingLinearInterpolator : public itk::VectorLinearInterpolateImageFunction<TImage, TScalar>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ClampingLinearInterpolator);

  using Self = ClampingLinearInterpolator;
  using Superclass = itk::VectorLinearInterpolateImageFunction<TImage, TScalar>;
  using Pointer = itk::SmartPointer<Self>;
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;
  using OutputType = typename Superclass::OutputType;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(ClampingLinearInterpolator);

  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override
  {
    ContinuousIndexType clamped = index;
    for (unsigned int d = 0; d < TImage::ImageDimension; ++d)
    {
      clamped[d] = std::clamp(clamped[d],
                              static_cast<typename ContinuousIndexType::ValueType>(this->GetStartIndex()[d]),
                              static_cast<typename ContinuousIndexType::ValueType>(this->GetEndIndex()[d]));
    }
    return Superclass::EvaluateAtContinuousIndex(clamped);
  }

protected:
  ClampingLinearInterpolator() = default;
};

template <typename TScalar>
class TransformPointTest
{
public:
  using TransformType = itk::DisplacementFieldTransform<TScalar, 3>;
  using FieldType = typename TransformType::DisplacementFieldType;
  using InterpolatorType = typename TransformType::InterpolatorType;
  using PointType = typename TransformType::InputPointType;

  // A field on a fixed geometry, buffered on the given region (by default the whole image).
  static typename FieldType::Pointer
  MakeField(double amplitude, const typename FieldType::RegionType & bufferedRegion = {})
  {
    auto                                 field = FieldType::New();
    const typename FieldType::RegionType largestRegion(typename FieldType::SizeType{ { 10, 12, 8 } });
    field->SetLargestPossibleRegion(largestRegion);
    field->SetBufferedRegion(bufferedRegion.GetNumberOfPixels() > 0 ? bufferedRegion : largestRegion);
    field->SetRequestedRegion(field->GetBufferedRegion());
    field->SetSpacing(itk::MakeVector(1.0, 1.5, 2.0));
    field->SetOrigin(itk::MakePoint(-2.0, 3.0, 1.0));
    auto rotation = itk::Euler3DTransform<itk::SpacePrecisionType>::New();
    rotation->SetRotation(0.1, -0.2, 0.3);
    field->SetDirection(rotation->GetMatrix());
    field->Allocate();
    for (itk::ImageRegionIteratorWithIndex<FieldType> it(field, field->GetBufferedRegion()); !it.IsAtEnd(); ++it)
    {
      const auto &                  index = it.GetIndex();
      typename FieldType::PixelType displacement;
      for (unsigned int d = 0; d < 3; ++d)
      {
        displacement[d] = static_cast<TScalar>(amplitude * std::sin(0.7 * index[(d + 1) % 3] + 0.3 * index[d] + d));
      }
      it.Set(displacement);
    }
    return field;
  }

  // Bounds from the interpolator's own image, then the continuous index of the point in the displacement field.
  static PointType
  Reference(const TransformType * transform, const PointType & inputPoint)
  {
    const InterpolatorType *             interpolator = transform->GetInterpolator();
    typename InterpolatorType::PointType point;
    point.CastFrom(inputPoint);
    PointType outputPoint;
    outputPoint.CastFrom(inputPoint);
    if (interpolator->IsInsideBuffer(point))
    {
      const typename InterpolatorType::ContinuousIndexType cidx =
        transform->GetDisplacementField()
          ->template TransformPhysicalPointToContinuousIndex<typename InterpolatorType::ContinuousIndexType::ValueType>(
            point);
      const auto displacement = interpolator->EvaluateAtContinuousIndex(cidx);
      for (unsigned int d = 0; d < 3; ++d)
      {
        outputPoint[d] += displacement[d];
      }
    }
    return outputPoint;
  }

  // Points inside the field and up to a few voxels outside it on every side.
  static void
  CheckPoints(const TransformType * transform, unsigned int & numberOutside)
  {
    std::mt19937                           generator(7);
    std::uniform_real_distribution<double> x(-8.0, 13.0);
    std::uniform_real_distribution<double> y(-3.0, 26.0);
    std::uniform_real_distribution<double> z(-5.0, 21.0);
    for (unsigned int n = 0; n < 2000; ++n)
    {
      const PointType input = itk::MakePoint(x(generator), y(generator), z(generator));
      const PointType expected = Reference(transform, input);
      const PointType actual = transform->TransformPoint(input);
      if (expected == input)
      {
        ++numberOutside;
      }
      for (unsigned int d = 0; d < 3; ++d)
      {
        ASSERT_EQ(actual[d], expected[d]) << "point " << input;
      }
    }
  }
};

template <typename TScalar>
void
CheckAllInterpolators()
{
  using Test = TransformPointTest<TScalar>;
  using TransformType = typename Test::TransformType;
  using FieldType = typename Test::FieldType;

  auto transform = TransformType::New();
  transform->SetDisplacementField(Test::MakeField(1.5));

  // Default linear interpolator: points outside the buffer pass through unchanged.
  unsigned int numberOutside = 0;
  ASSERT_NO_FATAL_FAILURE(Test::CheckPoints(transform, numberOutside));
  EXPECT_GT(numberOutside, 100U);
  EXPECT_LT(numberOutside, 1900U);

  // An extrapolating interpolator reports every point as inside, so every point is displaced.
  using ExtrapolatorType = itk::VectorLinearInterpolateNearestNeighborExtrapolateImageFunction<FieldType, TScalar>;
  transform->SetInterpolator(ExtrapolatorType::New());
  numberOutside = 0;
  ASSERT_NO_FATAL_FAILURE(Test::CheckPoints(transform, numberOutside));
  EXPECT_EQ(numberOutside, 0U);

  // Same geometry, smaller buffered region: the bounds come from the interpolator's image.
  using LinearType = itk::VectorLinearInterpolateImageFunction<FieldType, TScalar>;
  transform->SetInterpolator(LinearType::New());
  const typename FieldType::RegionType bufferedRegion(typename FieldType::IndexType{ { 2, 3, 1 } },
                                                      typename FieldType::SizeType{ { 6, 7, 5 } });
  auto                                 other = Test::MakeField(0.5, bufferedRegion);
  transform->GetModifiableInterpolator()->SetInputImage(other);
  numberOutside = 0;
  ASSERT_NO_FATAL_FAILURE(Test::CheckPoints(transform, numberOutside));
  EXPECT_GT(numberOutside, 100U);

  // Shifted origin: the field-index and physical-point bounds checks disagree, so this detects a missing fallback.
  using ClampingType = ClampingLinearInterpolator<FieldType, TScalar>;
  transform->SetInterpolator(ClampingType::New());
  auto shifted = Test::MakeField(0.5);
  shifted->SetOrigin(itk::MakePoint(1.0, 5.0, 4.0));
  transform->GetModifiableInterpolator()->SetInputImage(shifted);
  const typename Test::PointType probe = itk::MakePoint(-1.5, 3.5, 1.5);
  const auto                     fieldIndex =
    transform->GetDisplacementField()->template TransformPhysicalPointToContinuousIndex<double>(probe);
  ASSERT_TRUE(transform->GetDisplacementField()->GetLargestPossibleRegion().IsInside(fieldIndex));
  typename Test::InterpolatorType::PointType probePoint;
  probePoint.CastFrom(probe);
  ASSERT_FALSE(transform->GetInterpolator()->IsInsideBuffer(probePoint));
  EXPECT_EQ(transform->TransformPoint(probe), probe);
  numberOutside = 0;
  ASSERT_NO_FATAL_FAILURE(Test::CheckPoints(transform, numberOutside));
  EXPECT_GT(numberOutside, 100U);
}
} // namespace

TEST(DisplacementFieldTransform, TransformPointMatchesReferenceDouble) { CheckAllInterpolators<double>(); }

TEST(DisplacementFieldTransform, TransformPointMatchesReferenceFloat) { CheckAllInterpolators<float>(); }
