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

#include "itkAccumulateImageFilter.h"
#include "itkContinuousIndex.h"
#include "itkImage.h"
#include "itkMaximumProjectionImageFilter.h"

#include <gtest/gtest.h>

namespace
{

constexpr unsigned int Dimension = 3;
using ImageType = itk::Image<float, Dimension>;
using SizeType = ImageType::SizeType;
using IndexType = ImageType::IndexType;
using SpacingType = ImageType::SpacingType;
using PointType = ImageType::PointType;
using DirectionType = ImageType::DirectionType;
using RegionType = ImageType::RegionType;
using ContinuousIndexType = itk::ContinuousIndex<double, Dimension>;

constexpr double tolerance = 1e-9;

const SizeType    imageSize = { { 4, 5, 6 } };
const SpacingType imageSpacing{ { 1.5, 2.0, 2.5 } };
const PointType   imageOrigin{ { 10.0, 20.0, 30.0 } };

ImageType::Pointer
MakeImage(const IndexType & index, const DirectionType & direction)
{
  auto image = ImageType::New();
  image->SetRegions(RegionType(index, imageSize));
  image->SetSpacing(imageSpacing);
  image->SetOrigin(imageOrigin);
  image->SetDirection(direction);
  image->AllocateInitialized();
  return image;
}

DirectionType
IdentityDirection()
{
  DirectionType direction;
  direction.SetIdentity();
  return direction;
}

// Cyclic permutation of the axes; a proper rotation with determinant +1.
DirectionType
PermutationDirection()
{
  DirectionType direction;
  direction.Fill(0.0);
  direction(0, 1) = 1.0;
  direction(1, 2) = 1.0;
  direction(2, 0) = 1.0;
  return direction;
}

void
ExpectCollapsedPixelAtExtentCenter(const ImageType * input,
                                   const ImageType * output,
                                   unsigned int      collapsedAxis,
                                   const IndexType & sampleIndex)
{
  const SizeType  inputSize = input->GetLargestPossibleRegion().GetSize();
  const IndexType inputIndex = input->GetLargestPossibleRegion().GetIndex();

  ContinuousIndexType inputContinuousIndex;
  ContinuousIndexType outputContinuousIndex;
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    if (d == collapsedAxis)
    {
      inputContinuousIndex[d] = static_cast<double>(inputIndex[d]) + (static_cast<double>(inputSize[d]) - 1.0) / 2.0;
      outputContinuousIndex[d] = 0.0;
    }
    else
    {
      inputContinuousIndex[d] = static_cast<double>(sampleIndex[d]);
      outputContinuousIndex[d] = static_cast<double>(sampleIndex[d]);
    }
  }

  const PointType inputPoint = input->TransformContinuousIndexToPhysicalPoint<double>(inputContinuousIndex);
  const PointType outputPoint = output->TransformContinuousIndexToPhysicalPoint<double>(outputContinuousIndex);

  for (unsigned int d = 0; d < Dimension; ++d)
  {
    EXPECT_NEAR(outputPoint[d], inputPoint[d], tolerance) << "component " << d;
  }
}

void
ExpectCollapsedGeometry(const ImageType * output, unsigned int collapsedAxis)
{
  const PointType   outOrigin = output->GetOrigin();
  const SpacingType outSpacing = output->GetSpacing();
  const SizeType    outSize = output->GetLargestPossibleRegion().GetSize();

  for (unsigned int d = 0; d < Dimension; ++d)
  {
    if (d == collapsedAxis)
    {
      const double expectedOrigin = imageOrigin[d] + (static_cast<double>(imageSize[d]) - 1.0) * imageSpacing[d] / 2.0;
      EXPECT_NEAR(outOrigin[d], expectedOrigin, tolerance) << "axis " << d;
      EXPECT_NEAR(outSpacing[d], imageSpacing[d] * static_cast<double>(imageSize[d]), tolerance) << "axis " << d;
      EXPECT_EQ(outSize[d], 1u) << "axis " << d;
    }
    else
    {
      EXPECT_NEAR(outOrigin[d], imageOrigin[d], tolerance) << "axis " << d;
      EXPECT_NEAR(outSpacing[d], imageSpacing[d], tolerance) << "axis " << d;
      EXPECT_EQ(outSize[d], imageSize[d]) << "axis " << d;
    }
  }
}

} // namespace

TEST(ProjectionImageFilter, CollapsedAxisOriginIsCenteredForEveryAxis)
{
  const IndexType zeroIndex{};

  for (unsigned int projectionAxis = 0; projectionAxis < Dimension; ++projectionAxis)
  {
    const auto input = MakeImage(zeroIndex, IdentityDirection());

    const auto filter = itk::MaximumProjectionImageFilter<ImageType, ImageType>::New();
    filter->SetInput(input);
    filter->SetProjectionDimension(projectionAxis);
    filter->Update();

    const ImageType * output = filter->GetOutput();

    ExpectCollapsedGeometry(output, projectionAxis);
    ExpectCollapsedPixelAtExtentCenter(input, output, projectionAxis, IndexType{ { 1, 2, 3 } });
  }
}

TEST(ProjectionImageFilter, CollapsedAxisOriginFollowsDirectionAndStartIndex)
{
  constexpr unsigned int projectionAxis = 2;
  const IndexType        startIndex = { { 0, 0, 2 } };
  const DirectionType    direction = PermutationDirection();

  const auto input = MakeImage(startIndex, direction);

  const auto filter = itk::MaximumProjectionImageFilter<ImageType, ImageType>::New();
  filter->SetInput(input);
  filter->SetProjectionDimension(projectionAxis);
  filter->Update();

  const ImageType * output = filter->GetOutput();

  const double centerIndex =
    static_cast<double>(startIndex[projectionAxis]) + (static_cast<double>(imageSize[projectionAxis]) - 1.0) / 2.0;
  const double centerOffset = centerIndex * imageSpacing[projectionAxis];

  const PointType outOrigin = output->GetOrigin();
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    EXPECT_NEAR(outOrigin[d], imageOrigin[d] + direction(d, projectionAxis) * centerOffset, tolerance)
      << "component " << d;
  }

  ExpectCollapsedPixelAtExtentCenter(input, output, projectionAxis, IndexType{ { 1, 2, 2 } });
}

TEST(AccumulateImageFilter, OutOfRangeAccumulateDimensionThrows)
{
  const auto image = MakeImage(IndexType{}, IdentityDirection());

  using AccumulateFilterType = itk::AccumulateImageFilter<ImageType, ImageType>;
  auto filter = AccumulateFilterType::New();
  filter->SetInput(image);
  filter->SetAccumulateDimension(Dimension);
  EXPECT_THROW(filter->UpdateOutputInformation(), itk::ExceptionObject);
}

TEST(ProjectionImageFilter, OutOfRangeProjectionDimensionThrows)
{
  const auto image = MakeImage(IndexType{}, IdentityDirection());

  using ProjectionFilterType = itk::MaximumProjectionImageFilter<ImageType, ImageType>;
  auto filter = ProjectionFilterType::New();
  filter->SetInput(image);
  filter->SetProjectionDimension(Dimension);
  EXPECT_THROW(filter->UpdateOutputInformation(), itk::ExceptionObject);
}

TEST(AccumulateImageFilter, CollapsedAxisOriginIsCenteredForEveryAxis)
{
  const IndexType zeroIndex{};

  for (unsigned int accumulateAxis = 0; accumulateAxis < Dimension; ++accumulateAxis)
  {
    const auto input = MakeImage(zeroIndex, IdentityDirection());

    const auto filter = itk::AccumulateImageFilter<ImageType, ImageType>::New();
    filter->SetInput(input);
    filter->SetAccumulateDimension(accumulateAxis);
    filter->AverageOff();
    filter->Update();

    const ImageType * output = filter->GetOutput();

    ExpectCollapsedGeometry(output, accumulateAxis);
    ExpectCollapsedPixelAtExtentCenter(input, output, accumulateAxis, IndexType{ { 0, 2, 3 } });
  }
}
