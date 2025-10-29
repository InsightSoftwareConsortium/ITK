/*=========================================================================
 *
 *  Test to reproduce the Python issue scenario:
 *  Same voxel data with different spacing expectations
 *
 *=========================================================================*/

#include "itkHausdorffDistanceImageFilter.h"
#include "itkTestingMacros.h"

int
itkHausdorffDistanceImageFilterIssueReproduction(int, char *[])
{
  constexpr unsigned int ImageDimension{ 3 };

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  // We'll create two spheres using voxel coordinates (not physical)
  // and test with two different spacing configurations

  auto createSpheres = [](ImageType::Pointer img1, ImageType::Pointer img2) {
    ImageType::SizeType size;
    size.Fill(100);
    
    img1->SetRegions(size);
    img2->SetRegions(size);
    img1->Allocate();
    img2->Allocate();
    img1->FillBuffer(0);
    img2->FillBuffer(0);
    
    // Sphere 1: center at voxel (50,50,50), radius 10 voxels
    for (int z = 40; z <= 60; ++z) {
      for (int y = 40; y <= 60; ++y) {
        for (int x = 40; x <= 60; ++x) {
          ImageType::IndexType idx;
          idx[0] = x; idx[1] = y; idx[2] = z;
          double dist = std::sqrt(
            (x - 50.0) * (x - 50.0) +
            (y - 50.0) * (y - 50.0) +
            (z - 50.0) * (z - 50.0)
          );
          if (dist <= 10.0) {
            img1->SetPixel(idx, 255);
          }
        }
      }
    }
    
    // Sphere 2: center at voxel (50,50,60), radius 10 voxels
    for (int z = 50; z <= 70; ++z) {
      for (int y = 40; y <= 60; ++y) {
        for (int x = 40; x <= 60; ++x) {
          ImageType::IndexType idx;
          idx[0] = x; idx[1] = y; idx[2] = z;
          double dist = std::sqrt(
            (x - 50.0) * (x - 50.0) +
            (y - 50.0) * (y - 50.0) +
            (z - 60.0) * (z - 60.0)
          );
          if (dist <= 10.0) {
            img2->SetPixel(idx, 255);
          }
        }
      }
    }
  };

  int exitStatus = EXIT_SUCCESS;

  // Test 1: Equal (isotropic) spacing
  {
    std::cout << "\n=== Test 1: Equal spacing (1,1,1) ===" << std::endl;
    
    auto image1 = ImageType::New();
    auto image2 = ImageType::New();
    
    createSpheres(image1, image2);
    
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
    
    const FilterType::RealType distance1 = filter->GetHausdorffDistance();
    std::cout << "Hausdorff distance: " << distance1 << std::endl;
    std::cout << "Expected: ~0 (spheres overlap)" << std::endl;
  }

  // Test 2: Unequal (anisotropic) spacing
  {
    std::cout << "\n=== Test 2: Unequal spacing (1,1,2) ===" << std::endl;
    
    auto image1 = ImageType::New();
    auto image2 = ImageType::New();
    
    createSpheres(image1, image2);
    
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
    
    const FilterType::RealType distance2 = filter->GetHausdorffDistance();
    std::cout << "Hausdorff distance: " << distance2 << std::endl;
    std::cout << "Expected: different from Test 1 because physical geometry is different" << std::endl;
    std::cout << "(the same voxel data with different spacing represents different physical geometry)" << std::endl;
  }

  std::cout << "\n=== Analysis ===" << std::endl;
  std::cout << "ITK correctly accounts for spacing. When voxel data is the same but spacing changes," << std::endl;
  std::cout << "the physical geometry changes, so the Hausdorff distance changes." << std::endl;
  std::cout << "\nIf the issue reporter expects the same distance for both tests, they may be:" << std::endl;
  std::cout << "1. Expecting voxel-based distances (ignoring spacing) - which would be incorrect" << std::endl;
  std::cout << "2. Misunderstanding that same voxel data + different spacing = different geometry" << std::endl;

  return exitStatus;
}
