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
itkHausdorffDistanceImageFilterSamePhysicalGeometry(int, char *[])
{
  constexpr unsigned int ImageDimension{ 3 };

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  int exit_status = EXIT_SUCCESS;

  // Test: Same physical geometry with different voxel spacings
  // Create two pairs of images:
  // Pair 1: isotropic spacing (1,1,1)
  // Pair 2: anisotropic spacing (1,1,2)
  // Both pairs represent spheres at the same physical locations
  
  std::cout << "\n=== Test: Same physical geometry, different spacings ===" << std::endl;

  // Pair 1: Isotropic spacing (1,1,1)
  {
    std::cout << "\n--- Pair 1: Isotropic spacing (1,1,1) ---" << std::endl;
    
    auto image1 = ImageType::New();
    auto image2 = ImageType::New();

    ImageType::SizeType size;
    size.Fill(100);

    image1->SetRegions(size);
    image2->SetRegions(size);

    ImageType::SpacingType spacing;
    spacing.Fill(1.0);
    image1->SetSpacing(spacing);
    image2->SetSpacing(spacing);

    image1->Allocate();
    image2->Allocate();
    image1->FillBuffer(0);
    image2->FillBuffer(0);

    // Create sphere 1 centered at physical location (30,30,30) with radius 10
    // With spacing (1,1,1), this is at index (30,30,30)
    ImageType::IndexType center1;
    center1[0] = 30;
    center1[1] = 30;
    center1[2] = 30;
    
    for (int z = 0; z < 100; ++z) {
      for (int y = 0; y < 100; ++y) {
        for (int x = 0; x < 100; ++x) {
          ImageType::IndexType idx;
          idx[0] = x; idx[1] = y; idx[2] = z;
          
          // Convert to physical coordinates and check distance from center
          ImageType::PointType point;
          image1->TransformIndexToPhysicalPoint(idx, point);
          
          double dist = std::sqrt(
            (point[0] - 30.0) * (point[0] - 30.0) +
            (point[1] - 30.0) * (point[1] - 30.0) +
            (point[2] - 30.0) * (point[2] - 30.0)
          );
          
          if (dist <= 10.0) {
            image1->SetPixel(idx, 255);
          }
        }
      }
    }

    // Create sphere 2 centered at physical location (30,30,40) with radius 10
    // Shifted by 10 in z direction
    for (int z = 0; z < 100; ++z) {
      for (int y = 0; y < 100; ++y) {
        for (int x = 0; x < 100; ++x) {
          ImageType::IndexType idx;
          idx[0] = x; idx[1] = y; idx[2] = z;
          
          ImageType::PointType point;
          image2->TransformIndexToPhysicalPoint(idx, point);
          
          double dist = std::sqrt(
            (point[0] - 30.0) * (point[0] - 30.0) +
            (point[1] - 30.0) * (point[1] - 30.0) +
            (point[2] - 40.0) * (point[2] - 40.0)
          );
          
          if (dist <= 10.0) {
            image2->SetPixel(idx, 255);
          }
        }
      }
    }

    using FilterType = itk::HausdorffDistanceImageFilter<ImageType, ImageType>;
    auto filter = FilterType::New();
    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->SetUseImageSpacing(true);
    filter->Update();

    const FilterType::RealType distance1 = filter->GetHausdorffDistance();
    std::cout << "Hausdorff distance: " << distance1 << std::endl;

    // Pair 2: Anisotropic spacing (1,1,2)
    std::cout << "\n--- Pair 2: Anisotropic spacing (1,1,2) ---" << std::endl;
    
    auto image3 = ImageType::New();
    auto image4 = ImageType::New();

    // For anisotropic spacing (1,1,2), we need a different grid size in z
    // to cover the same physical space
    ImageType::SizeType size2;
    size2[0] = 100;
    size2[1] = 100;
    size2[2] = 50;  // Half the z resolution because spacing is 2x

    image3->SetRegions(size2);
    image4->SetRegions(size2);

    ImageType::SpacingType spacing2;
    spacing2[0] = 1.0;
    spacing2[1] = 1.0;
    spacing2[2] = 2.0;
    image3->SetSpacing(spacing2);
    image4->SetSpacing(spacing2);

    image3->Allocate();
    image4->Allocate();
    image3->FillBuffer(0);
    image4->FillBuffer(0);

    // Create sphere 1 centered at same physical location (30,30,30) with radius 10
    for (int z = 0; z < 50; ++z) {
      for (int y = 0; y < 100; ++y) {
        for (int x = 0; x < 100; ++x) {
          ImageType::IndexType idx;
          idx[0] = x; idx[1] = y; idx[2] = z;
          
          ImageType::PointType point;
          image3->TransformIndexToPhysicalPoint(idx, point);
          
          double dist = std::sqrt(
            (point[0] - 30.0) * (point[0] - 30.0) +
            (point[1] - 30.0) * (point[1] - 30.0) +
            (point[2] - 30.0) * (point[2] - 30.0)
          );
          
          if (dist <= 10.0) {
            image3->SetPixel(idx, 255);
          }
        }
      }
    }

    // Create sphere 2 centered at same physical location (30,30,40) with radius 10
    for (int z = 0; z < 50; ++z) {
      for (int y = 0; y < 100; ++y) {
        for (int x = 0; x < 100; ++x) {
          ImageType::IndexType idx;
          idx[0] = x; idx[1] = y; idx[2] = z;
          
          ImageType::PointType point;
          image4->TransformIndexToPhysicalPoint(idx, point);
          
          double dist = std::sqrt(
            (point[0] - 30.0) * (point[0] - 30.0) +
            (point[1] - 30.0) * (point[1] - 30.0) +
            (point[2] - 40.0) * (point[2] - 40.0)
          );
          
          if (dist <= 10.0) {
            image4->SetPixel(idx, 255);
          }
        }
      }
    }

    auto filter2 = FilterType::New();
    filter2->SetInput1(image3);
    filter2->SetInput2(image4);
    filter2->SetUseImageSpacing(true);
    filter2->Update();

    const FilterType::RealType distance2 = filter2->GetHausdorffDistance();
    std::cout << "Hausdorff distance: " << distance2 << std::endl;

    // The distances should be approximately equal since they represent
    // the same geometry in physical space
    std::cout << "\nComparison:" << std::endl;
    std::cout << "Distance with isotropic spacing:   " << distance1 << std::endl;
    std::cout << "Distance with anisotropic spacing: " << distance2 << std::endl;
    std::cout << "Difference: " << std::abs(distance1 - distance2) << std::endl;

    if (std::abs(distance1 - distance2) > 0.5)
    {
      std::cout << "Test FAILED. Distances should be approximately equal for same physical geometry." << std::endl;
      exit_status = EXIT_FAILURE;
    }
    else
    {
      std::cout << "Test PASSED." << std::endl;
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
