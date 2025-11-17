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

#include "itkContourMeanDistanceImageFilter.h"
#include "itkImageRegionRange.h"
#include "itkSimpleFilterWatcher.h"
#include "itkGTest.h"

#include <algorithm> // For fill.
#include <numeric>   // For iota.

TEST(ContourMeanDistanceImageFilter, Test)
{
  using Pixel1Type = unsigned int;
  using Pixel2Type = float;
  enum
  {
    ImageDimension = 3
  };

  using Image1Type = itk::Image<Pixel1Type, ImageDimension>;
  using Image2Type = itk::Image<Pixel2Type, ImageDimension>;

  auto image1 = Image1Type::New();
  auto image2 = Image2Type::New();

  auto size = Image1Type::SizeType::Filled(50);

  image1->SetRegions(size);
  image2->SetRegions(size);

  image1->AllocateInitialized();
  image2->AllocateInitialized();

  using RegionType = Image1Type::RegionType;

  using IndexType = Image1Type::IndexType;
  IndexType index;

  size.Fill(20);
  index.Fill(10);
  RegionType region1 = { index, size };

  size.Fill(15);
  index.Fill(20);
  RegionType region2 = { index, size };

  const itk::ImageRegionRange<Image1Type> imageRegionRange1(*image1, region1);
  std::iota(imageRegionRange1.begin(), imageRegionRange1.end(), Pixel1Type{ 1 });

  const itk::ImageRegionRange<Image2Type> imageRegionRange2(*image2, region2);
  std::fill(imageRegionRange2.begin(), imageRegionRange2.end(), Pixel2Type{ 7.2 });


  // compute the directed Mean distance h(image1,image2)
  {
    using FilterType = itk::ContourMeanDistanceImageFilter<Image1Type, Image2Type>;
    auto filter = FilterType::New();

    ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(filter, ContourMeanDistanceImageFilter, ImageToImageFilter);


    const itk::SimpleFilterWatcher watcher(filter, "filter");

    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->Update();


    // check results

    constexpr FilterType::RealType trueDistance{ 8.07158 };
    // std::sqrt( double{ ImageDimension } );
    const FilterType::RealType distance = filter->GetMeanDistance();

    EXPECT_NEAR(distance, trueDistance, 0.5);
  }

  // compute the directed Mean distance h(image2,image1)
  {
    using FilterType = itk::ContourMeanDistanceImageFilter<Image2Type, Image1Type>;
    auto filter = FilterType::New();

    filter->SetInput1(image2);
    filter->SetInput2(image1);
    filter->Update();


    // check results
    constexpr FilterType::RealType trueDistance{ 8.07158 };
    const FilterType::RealType     distance = filter->GetMeanDistance();

    EXPECT_NEAR(distance, trueDistance, 0.5);
  }

  // compute the directed Mean distance h(image2,image1) with different pixel sizes
  {
    using FilterType = itk::ContourMeanDistanceImageFilter<Image2Type, Image1Type>;
    auto filter = FilterType::New();

    constexpr bool useImageSpacing{ true };
    ITK_GTEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

    constexpr itk::SpacePrecisionType spacingValue{ 0.5 };
    constexpr auto spacing = itk::MakeFilled<itk::Vector<itk::SpacePrecisionType, ImageDimension>>(spacingValue);
    image1->SetSpacing(spacing);
    image2->SetSpacing(spacing);

    filter->SetInput1(image2);
    filter->SetInput2(image1);
    filter->Update();

    // check results
    constexpr FilterType::RealType trueDistance{ 8.07158 / 2 };
    const FilterType::RealType     distance = filter->GetMeanDistance();
    EXPECT_NEAR(distance, trueDistance, 0.5);
  }
}
