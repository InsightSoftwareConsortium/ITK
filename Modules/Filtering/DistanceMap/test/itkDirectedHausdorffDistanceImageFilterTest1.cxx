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

#include "itkDirectedHausdorffDistanceImageFilter.h"
#include "itkImageRegionRange.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

#include <algorithm> // For fill.
#include <numeric>   // For iota.

int
itkDirectedHausdorffDistanceImageFilterTest1(int, char *[])
{
  constexpr unsigned int ImageDimension{ 3 };

  using Pixel1Type = unsigned int;
  using Pixel2Type = float;

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
  IndexType index{ 10, 10, 10 };
  size.Fill(20);
  RegionType region1 = { index, size };

  size.Fill(15);
  index.Fill(20);
  RegionType region2 = { index, size };

  const itk::ImageRegionRange<Image1Type> imageRegionRange1(*image1, region1);
  std::iota(imageRegionRange1.begin(), imageRegionRange1.end(), Pixel1Type{ 1 });

  const itk::ImageRegionRange<Image2Type> imageRegionRange2(*image2, region2);
  std::fill(imageRegionRange2.begin(), imageRegionRange2.end(), Pixel2Type{ 7.2 });

  // If no failures detected, then EXIT_SUCCESS
  int exit_status = EXIT_SUCCESS;

  // Compute the directed Hausdorff distance h(image1,image2)
  {
    using FilterType = itk::DirectedHausdorffDistanceImageFilter<Image1Type, Image2Type>;
    auto                           filter = FilterType::New();
    const itk::SimpleFilterWatcher watcher(filter, "filter");

    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->Update();

    // Check results
    const FilterType::RealType trueDistance = 10 * std::sqrt(double{ ImageDimension });
    const FilterType::RealType distance = filter->GetDirectedHausdorffDistance();

    std::cout << " True distance: " << trueDistance << std::endl;
    std::cout << " Computed computed: " << distance << std::endl;
    std::cout << " Average distance: " << filter->GetAverageHausdorffDistance() << std::endl;
    if (itk::Math::abs(trueDistance - distance) > 0.1)
    {
      std::cout << "Test failed. " << std::endl;
      exit_status = EXIT_FAILURE;
    }
    if (itk::Math::abs(6.5 - filter->GetAverageHausdorffDistance()) > 0.1)
    {
      std::cout << "Test failed, average distance incorrect. " << std::endl;
      exit_status = EXIT_FAILURE;
    }
  }

  // Compute the directed Hausdorff distance h(image2,image1)
  {
    using FilterType = itk::DirectedHausdorffDistanceImageFilter<Image2Type, Image1Type>;
    auto filter = FilterType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, DirectedHausdorffDistanceImageFilter, ImageToImageFilter);

    filter->SetInput1(image2);
    filter->SetInput2(image1);
    filter->Update();

    // Check results
    const FilterType::RealType trueDistance = 5 * std::sqrt(double{ ImageDimension });
    const FilterType::RealType distance = filter->GetDirectedHausdorffDistance();

    std::cout << " True distance: " << trueDistance << std::endl;
    std::cout << " Computed computed: " << distance << std::endl;
    std::cout << " Average distance: " << filter->GetAverageHausdorffDistance() << std::endl;

    if (itk::Math::abs(trueDistance - distance) > 0.1)
    {
      std::cout << "Test failed. " << std::endl;
      exit_status = EXIT_FAILURE;
    }
    if (itk::Math::abs(2.5 - filter->GetAverageHausdorffDistance()) > 0.1)
    {
      std::cout << "Test failed, average distance incorrect. " << std::endl;
      exit_status = EXIT_FAILURE;
    }
  }

  if (exit_status == EXIT_SUCCESS)
  {
    std::cout << "All tests passed. " << std::endl;
  }
  else
  {
    std::cout << "Some test failed. " << std::endl;
  }
  return exit_status;
}
