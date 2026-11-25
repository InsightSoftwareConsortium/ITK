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

#include "itkGridImageSource.h"
#include "itkImage.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkGTest.h"

#include <algorithm>
#include <cmath>

namespace
{
constexpr unsigned int Dimension = 2;
using ImageType = itk::Image<float, Dimension>;
using GridSourceType = itk::GridImageSource<ImageType>;

ImageType::Pointer
MakeGrid(const double originValue)
{
  auto source = GridSourceType::New();
  source->SetSize(itk::MakeFilled<ImageType::SizeType>(32));
  source->SetOrigin(itk::MakeFilled<ImageType::PointType>(originValue));
  source->Update();
  ImageType::Pointer output = source->GetOutput();
  output->DisconnectPipeline();
  return output;
}

double
MaxAbsoluteDifference(ImageType * imageA, ImageType * imageB)
{
  using ComparisonFilterType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto comparison = ComparisonFilterType::New();
  comparison->SetValidInput(imageA);
  comparison->SetTestInput(imageB);
  // The two grids occupy different physical locations by design; compare per-voxel.
  comparison->SetVerifyInputInformation(false);
  comparison->Update();
  return static_cast<double>(comparison->GetMaximumDifference());
}
} // namespace

TEST(GridImageSource, OriginShiftsGridPattern)
{
  const ImageType::Pointer unshifted = MakeGrid(0.0);
  const ImageType::Pointer shifted = MakeGrid(2.0);

  constexpr double minimumExpectedDifference = 10.0;
  EXPECT_GT(MaxAbsoluteDifference(unshifted, shifted), minimumExpectedDifference);
}

TEST(GridImageSource, OriginShiftByGridPeriodLeavesPatternUnchanged)
{
  // Grid lines are anchored in physical space, so translating the image by a
  // whole grid period (default GridSpacing is 4.0) samples the same pattern.
  const ImageType::Pointer unshifted = MakeGrid(0.0);
  const ImageType::Pointer periodShifted = MakeGrid(4.0);

  EXPECT_LT(MaxAbsoluteDifference(unshifted, periodShifted), 1e-4);
}

TEST(GridImageSource, FarFromPhysicalOriginStillProducesGrid)
{
  const ImageType::Pointer farGrid = MakeGrid(1000.0);

  float minValue = itk::NumericTraits<float>::max();
  float maxValue = itk::NumericTraits<float>::NonpositiveMin();
  for (itk::ImageRegionConstIterator<ImageType> it(farGrid, farGrid->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    ASSERT_FALSE(std::isnan(it.Get()));
    minValue = std::min(minValue, it.Get());
    maxValue = std::max(maxValue, it.Get());
  }
  // A grid pattern must span most of the default [0, 255] scale range.
  EXPECT_LT(minValue, 10.0f);
  EXPECT_GT(maxValue, 200.0f);
}
