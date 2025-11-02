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
#include "itkTestingMacros.h"

#include <algorithm> // For fill.
#include <numeric>   // For iota.

int
itkContourMeanDistanceImageFilterTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " useImageSpacing" << std::endl;
    return EXIT_FAILURE;
  }

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

    ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ContourMeanDistanceImageFilter, ImageToImageFilter);


    const itk::SimpleFilterWatcher watcher(filter, "filter");

    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->Update();


    // check results

    constexpr FilterType::RealType trueDistance{ 8.07158 };
    // std::sqrt( double{ ImageDimension } );
    const FilterType::RealType distance = filter->GetMeanDistance();

    std::cout << " True     distance: " << trueDistance << std::endl;
    std::cout << " Computed distance: " << distance << std::endl;

    if (itk::Math::abs(trueDistance - distance) > 0.5)
    {
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
    }
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

    std::cout << " True     distance: " << trueDistance << std::endl;
    std::cout << " Computed distance: " << distance << std::endl;

    if (itk::Math::abs(trueDistance - distance) > 0.5)
    {
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
    }
  }

  // compute the directed Mean distance h(image2,image1) with different pixel sizes
  {
    using FilterType = itk::ContourMeanDistanceImageFilter<Image2Type, Image1Type>;
    auto filter = FilterType::New();

    auto useImageSpacing = static_cast<bool>(std::stoi(argv[1]));
    ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

    Image1Type::SpacingType spacing1 = image1->GetSpacing();
    spacing1[0] = spacing1[0] / 2;
    spacing1[1] = spacing1[1] / 2;
    spacing1[2] = spacing1[2] / 2;
    image1->SetSpacing(spacing1);

    Image2Type::SpacingType spacing2 = image2->GetSpacing();
    spacing2[0] = spacing2[0] / 2;
    spacing2[1] = spacing2[1] / 2;
    spacing2[2] = spacing2[2] / 2;
    image2->SetSpacing(spacing2);

    filter->SetInput1(image2);
    filter->SetInput2(image1);
    filter->Update();

    // check results
    constexpr FilterType::RealType trueDistance{ 8.07158 / 2 };
    const FilterType::RealType     distance = filter->GetMeanDistance();
    std::cout << " True     distance: " << trueDistance << std::endl;
    std::cout << " Computed distance: " << distance << std::endl;
    if (itk::Math::abs(trueDistance - distance) > 0.5)
    {
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;
}
