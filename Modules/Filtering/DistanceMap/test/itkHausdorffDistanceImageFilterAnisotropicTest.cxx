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

#include "itkHausdorffDistanceImageFilter.h"
#include "itkTestingMacros.h"

int
itkHausdorffDistanceImageFilterAnisotropicTest(int, char *[])
{
  constexpr unsigned int ImageDimension{ 3 };

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  // Create two images
  auto image1 = ImageType::New();
  auto image2 = ImageType::New();

  auto size = ImageType::SizeType::Filled(50);

  image1->SetRegions(size);
  image2->SetRegions(size);

  image1->Allocate();
  image2->Allocate();

  image1->FillBuffer(PixelType{});
  image2->FillBuffer(PixelType{});

  // Create two non-overlapping rectangular regions
  // Region 1: from (10,10,10) to (30,30,30) - a 20x20x20 box
  // Region 2: from (10,10,35) to (30,30,45) - a 20x20x10 box, shifted in z
  
  using RegionType = ImageType::RegionType;
  using IndexType = ImageType::IndexType;

  IndexType index1{ 10, 10, 10 };
  size.Fill(20);
  RegionType region1 = { index1, size };

  IndexType index2{ 10, 10, 35 };
  ImageType::SizeType size2;
  size2[0] = 20;
  size2[1] = 20;
  size2[2] = 10;
  RegionType region2 = { index2, size2 };

  // Fill the regions with non-zero values
  itk::ImageRegionIterator<ImageType> it1(image1, region1);
  while (!it1.IsAtEnd())
  {
    it1.Set(255);
    ++it1;
  }

  itk::ImageRegionIterator<ImageType> it2(image2, region2);
  while (!it2.IsAtEnd())
  {
    it2.Set(255);
    ++it2;
  }

  int exit_status = EXIT_SUCCESS;

  // Test 1: Isotropic spacing (1.0, 1.0, 1.0)
  {
    std::cout << "\n=== Test 1: Isotropic spacing (1.0, 1.0, 1.0) ===" << std::endl;
    
    ImageType::SpacingType spacing;
    spacing.Fill(1.0);
    image1->SetSpacing(spacing);
    image2->SetSpacing(spacing);

    using FilterType = itk::HausdorffDistanceImageFilter<ImageType, ImageType>;
    auto filter = FilterType::New();

    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->SetUseImageSpacing(true);
    filter->Update();

    const FilterType::RealType distance = filter->GetHausdorffDistance();
    
    // Box1 in physical space: (10,10,10) to (29,29,29)
    // Box2 in physical space: (10,10,35) to (29,29,44)
    // Farthest point in box1 from box2: (10,10,10) to nearest point on box2 (10,10,35) = 25
    // Farthest point in box2 from box1: (10,10,44) to nearest point on box1 (10,10,29) = 15
    // Hausdorff distance is max(25, 15) = 25
    
    const FilterType::RealType expectedDistance = 25.0;
    
    std::cout << "  Expected distance: " << expectedDistance << std::endl;
    std::cout << "  Computed distance: " << distance << std::endl;
    
    if (itk::Math::abs(expectedDistance - distance) > 0.1)
    {
      std::cout << "  Test FAILED. Distance mismatch." << std::endl;
      exit_status = EXIT_FAILURE;
    }
    else
    {
      std::cout << "  Test PASSED." << std::endl;
    }
  }

  // Test 2: Anisotropic spacing (1.0, 1.0, 2.0)
  {
    std::cout << "\n=== Test 2: Anisotropic spacing (1.0, 1.0, 2.0) ===" << std::endl;
    
    ImageType::SpacingType spacing;
    spacing[0] = 1.0;
    spacing[1] = 1.0;
    spacing[2] = 2.0;
    image1->SetSpacing(spacing);
    image2->SetSpacing(spacing);

    using FilterType = itk::HausdorffDistanceImageFilter<ImageType, ImageType>;
    auto filter = FilterType::New();

    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->SetUseImageSpacing(true);
    filter->Update();

    const FilterType::RealType distance = filter->GetHausdorffDistance();
    
    // With spacing (1.0, 1.0, 2.0):
    // Box1 in index space: (10,10,10) to (29,29,29)
    // Box1 in physical space: (10,10,20) to (29,29,58)
    // Box2 in index space: (10,10,35) to (29,29,44)
    // Box2 in physical space: (10,10,70) to (29,29,88)
    // Farthest point in box1 from box2: (10,10,20) to nearest on box2 (10,10,70) = 50
    // Farthest point in box2 from box1: (10,10,88) to nearest on box1 (10,10,58) = 30
    // Hausdorff distance is max(50, 30) = 50
    
    const FilterType::RealType expectedDistance = 50.0;
    
    std::cout << "  Expected distance: " << expectedDistance << std::endl;
    std::cout << "  Computed distance: " << distance << std::endl;
    
    if (itk::Math::abs(expectedDistance - distance) > 0.1)
    {
      std::cout << "  Test FAILED. Distance mismatch." << std::endl;
      exit_status = EXIT_FAILURE;
    }
    else
    {
      std::cout << "  Test PASSED." << std::endl;
    }
  }

  // Test 3: Different anisotropic spacing (2.0, 1.0, 1.0)
  {
    std::cout << "\n=== Test 3: Anisotropic spacing (2.0, 1.0, 1.0) ===" << std::endl;
    
    ImageType::SpacingType spacing;
    spacing[0] = 2.0;
    spacing[1] = 1.0;
    spacing[2] = 1.0;
    image1->SetSpacing(spacing);
    image2->SetSpacing(spacing);

    using FilterType = itk::HausdorffDistanceImageFilter<ImageType, ImageType>;
    auto filter = FilterType::New();

    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->SetUseImageSpacing(true);
    filter->Update();

    const FilterType::RealType distance = filter->GetHausdorffDistance();
    
    // With spacing (2.0, 1.0, 1.0):
    // Box1 in index space: (10,10,10) to (29,29,29)
    // Box1 in physical space: (20,10,10) to (58,29,29)
    // Box2 in index space: (10,10,35) to (29,29,44)
    // Box2 in physical space: (20,10,35) to (58,29,44)
    // Farthest point in box1 from box2: (20,10,10) to nearest on box2 (20,10,35) = 25
    // Farthest point in box2 from box1: (20,10,44) to nearest on box1 (20,10,29) = 15
    // Hausdorff distance is max(25, 15) = 25
    
    const FilterType::RealType expectedDistance = 25.0;
    
    std::cout << "  Expected distance: " << expectedDistance << std::endl;
    std::cout << "  Computed distance: " << distance << std::endl;
    
    if (itk::Math::abs(expectedDistance - distance) > 0.1)
    {
      std::cout << "  Test FAILED. Distance mismatch." << std::endl;
      exit_status = EXIT_FAILURE;
    }
    else
    {
      std::cout << "  Test PASSED." << std::endl;
    }
  }

  if (exit_status == EXIT_SUCCESS)
  {
    std::cout << "\n=== All tests passed ===" << std::endl;
  }
  else
  {
    std::cout << "\n=== Some tests failed ===" << std::endl;
  }
  
  return exit_status;
}
