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

#include "itkInverseDisplacementFieldImageFilter.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkGTest.h"
#include <algorithm> // For max.

namespace
{
using VectorType = itk::Vector<float, 2>;
using FieldType = itk::Image<VectorType, 2>;
using FilterType = itk::InverseDisplacementFieldImageFilter<FieldType, FieldType>;

FieldType::DirectionType
MakeRotation()
{
  FieldType::DirectionType direction;
  direction(0, 0) = 0.0;
  direction(0, 1) = -1.0;
  direction(1, 0) = 1.0;
  direction(1, 1) = 0.0;
  return direction;
}
} // namespace

// The kernel-spline subsampler must inherit the input direction; the exact inverse of a
// constant displacement is its negation, reproduced exactly by the spline's affine part
// (issue #6575, B32).
TEST(InverseDisplacementFieldImageFilter, RecoversConstantInverseOnRotatedField)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(32) });
  field->SetDirection(MakeRotation());
  field->Allocate();
  field->FillBuffer(itk::MakeVector(2.0F, 3.0F));

  auto filter = FilterType::New();
  filter->SetInput(field);
  filter->SetKernelTransform(itk::ThinPlateSplineKernelTransform<double, 2>::New());
  filter->SetSubsamplingFactor(4);
  filter->SetSize(FieldType::SizeType::Filled(16));
  filter->SetOutputSpacing(itk::MakeFilled<FieldType::SpacingType>(1.0));
  // Output window inside the subsampled landmark hull, so no sample extrapolates.
  filter->SetOutputOrigin(itk::MakePoint(-16.0, 8.0));
  filter->UpdateLargestPossibleRegion();

  const FieldType *                        output = filter->GetOutput();
  itk::ImageRegionConstIterator<FieldType> it(output, output->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    EXPECT_NEAR(it.Get()[0], -2.0, 0.05);
    EXPECT_NEAR(it.Get()[1], -3.0, 0.05);
  }
}

// Unless SetOutputDirection is called, the output inherits the input direction.
TEST(InverseDisplacementFieldImageFilter, OutputDirectionDefaultsToInputDirection)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(32) });
  field->SetDirection(MakeRotation());
  field->Allocate();
  field->FillBuffer(itk::MakeVector(2.0F, 3.0F));

  auto filter = FilterType::New();
  filter->SetInput(field);
  filter->SetKernelTransform(itk::ThinPlateSplineKernelTransform<double, 2>::New());
  filter->SetSubsamplingFactor(4);
  filter->SetSize(FieldType::SizeType::Filled(16));
  filter->SetOutputSpacing(itk::MakeFilled<FieldType::SpacingType>(1.0));
  filter->SetOutputOrigin(itk::MakePoint(-16.0, 8.0));
  filter->UpdateOutputInformation();

  EXPECT_EQ(filter->GetOutput()->GetDirection(), MakeRotation());
}

// An explicitly requested output direction is honored and the inverse is evaluated
// consistently on that grid: the inverse of a constant displacement is its negation.
TEST(InverseDisplacementFieldImageFilter, HonorsExplicitOutputDirection)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(32) });
  field->SetDirection(MakeRotation());
  field->Allocate();
  field->FillBuffer(itk::MakeVector(2.0F, 3.0F));

  auto filter = FilterType::New();
  filter->SetInput(field);
  filter->SetKernelTransform(itk::ThinPlateSplineKernelTransform<double, 2>::New());
  filter->SetSubsamplingFactor(4);
  filter->SetSize(FieldType::SizeType::Filled(16));
  filter->SetOutputSpacing(itk::MakeFilled<FieldType::SpacingType>(1.0));
  // Axis-aligned output window inside the subsampled landmark hull.
  filter->SetOutputOrigin(itk::MakePoint(-16.0, 8.0));
  FieldType::DirectionType identity;
  identity.SetIdentity();
  filter->SetOutputDirection(identity);
  filter->UpdateLargestPossibleRegion();

  const FieldType * output = filter->GetOutput();
  EXPECT_EQ(output->GetDirection(), identity);
  itk::ImageRegionConstIterator<FieldType> it(output, output->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    EXPECT_NEAR(it.Get()[0], -2.0, 0.05);
    EXPECT_NEAR(it.Get()[1], -3.0, 0.05);
  }
}

// The landmark lattice starts at fine index (k-1)/2 so unsampled margins are balanced;
// for a nonlinear field this bounds the round-trip error near the high edge, where the
// uncentered lattice (samples at 0, k, ..., N-k) had to extrapolate over k-1 voxels.
TEST(InverseDisplacementFieldImageFilter, CenteredSubsamplingBoundsHighEdgeError)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(32) });
  field->Allocate();
  for (itk::ImageRegionIteratorWithIndex<FieldType> it(field, field->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const auto index = it.GetIndex();
    it.Set(itk::MakeVector(0.002F * index[0] * index[0], 0.002F * index[1] * index[1]));
  }

  auto filter = FilterType::New();
  filter->SetInput(field);
  filter->SetKernelTransform(itk::ThinPlateSplineKernelTransform<double, 2>::New());
  filter->SetSubsamplingFactor(4);
  filter->SetSize(FieldType::SizeType::Filled(32));
  filter->SetOutputSpacing(itk::MakeFilled<FieldType::SpacingType>(1.0));
  filter->SetOutputOrigin(itk::MakePoint(0.0, 0.0));
  filter->UpdateLargestPossibleRegion();

  const FieldType * output = filter->GetOutput();
  double            maxError = 0.0;
  for (itk::ImageRegionConstIteratorWithIndex<FieldType> it(output, output->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    FieldType::PointType point;
    output->TransformIndexToPhysicalPoint(it.GetIndex(), point);
    // Round trip: apply the inverse displacement, then the analytic forward field.
    itk::Point<double, 2> back;
    for (unsigned int i = 0; i < 2; ++i)
    {
      back[i] = point[i] + it.Get()[i];
      back[i] += 0.002 * back[i] * back[i];
      maxError = std::max(maxError, itk::Math::Absolute(back[i] - point[i]));
    }
  }
  EXPECT_LT(maxError, 0.06);
}

// An input smaller than the subsampling factor yields a zero-size landmark lattice; the
// filter must reject it up front instead of building a spline with no landmarks.
TEST(InverseDisplacementFieldImageFilter, RejectsInputSmallerThanSubsamplingFactor)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(8) });
  field->Allocate();
  field->FillBuffer(itk::MakeVector(0.0F, 0.0F));

  auto filter = FilterType::New();
  filter->SetInput(field);
  filter->SetKernelTransform(itk::ThinPlateSplineKernelTransform<double, 2>::New());
  filter->SetSubsamplingFactor(16);
  filter->SetSize(FieldType::SizeType::Filled(8));
  filter->SetOutputSpacing(itk::MakeFilled<FieldType::SpacingType>(1.0));
  filter->SetOutputOrigin(itk::MakePoint(0.0, 0.0));
  EXPECT_THROW(filter->UpdateLargestPossibleRegion(), itk::ExceptionObject);

  filter->SetSubsamplingFactor(0);
  EXPECT_THROW(filter->UpdateLargestPossibleRegion(), itk::ExceptionObject);

  filter->SetSubsamplingFactor(4);
  EXPECT_NO_THROW(filter->UpdateLargestPossibleRegion());
}

// The centered lattice must account for a non-zero input region start index: the
// landmarks are placed relative to the region start, not the absolute index 0, so a
// cropped/non-zero-start input inverts as accurately as a zero-start one (issue #6582
// follow-up; greptile P1). The input below occupies physical [0,31] via index [5,36]
// with a compensating origin, matching CenteredSubsamplingBoundsHighEdgeError's grid.
TEST(InverseDisplacementFieldImageFilter, NonZeroStartRegionInvertsCorrectly)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ itk::MakeIndex(5, 5), FieldType::SizeType::Filled(32) });
  field->SetOrigin(itk::MakePoint(-5.0, -5.0));
  field->Allocate();
  for (itk::ImageRegionIteratorWithIndex<FieldType> it(field, field->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    FieldType::PointType p;
    field->TransformIndexToPhysicalPoint(it.GetIndex(), p);
    it.Set(itk::MakeVector(static_cast<float>(0.002 * p[0] * p[0]), static_cast<float>(0.002 * p[1] * p[1])));
  }

  auto filter = FilterType::New();
  filter->SetInput(field);
  filter->SetKernelTransform(itk::ThinPlateSplineKernelTransform<double, 2>::New());
  filter->SetSubsamplingFactor(4);
  filter->SetSize(FieldType::SizeType::Filled(32));
  filter->SetOutputSpacing(itk::MakeFilled<FieldType::SpacingType>(1.0));
  filter->SetOutputOrigin(itk::MakePoint(0.0, 0.0));
  filter->UpdateLargestPossibleRegion();

  const FieldType * output = filter->GetOutput();
  double            maxError = 0.0;
  for (itk::ImageRegionConstIteratorWithIndex<FieldType> it(output, output->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    FieldType::PointType point;
    output->TransformIndexToPhysicalPoint(it.GetIndex(), point);
    itk::Point<double, 2> back;
    for (unsigned int i = 0; i < 2; ++i)
    {
      back[i] = point[i] + it.Get()[i];
      back[i] += 0.002 * back[i] * back[i];
      maxError = std::max(maxError, itk::Math::Absolute(back[i] - point[i]));
    }
  }
  EXPECT_LT(maxError, 0.06);
}
