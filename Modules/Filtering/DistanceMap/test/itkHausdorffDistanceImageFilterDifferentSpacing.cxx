/*=========================================================================
 *
 *  Test HausdorffDistanceImageFilter with DIFFERENT spacing on two images
 *
 *=========================================================================*/

#include "itkHausdorffDistanceImageFilter.h"
#include "itkTestingMacros.h"

int
itkHausdorffDistanceImageFilterDifferentSpacing(int, char *[])
{
  constexpr unsigned int ImageDimension{ 3 };

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  // Create two images with THE SAME voxel data but DIFFERENT spacing
  auto image1 = ImageType::New();
  auto image2 = ImageType::New();

  ImageType::SizeType size;
  size.Fill(50);

  image1->SetRegions(size);
  image2->SetRegions(size);

  // Set DIFFERENT spacing for the two images
  ImageType::SpacingType spacing1;
  spacing1.Fill(1.0);
  image1->SetSpacing(spacing1);

  ImageType::SpacingType spacing2;
  spacing2[0] = 1.0;
  spacing2[1] = 1.0;
  spacing2[2] = 2.0;  // Different in z!
  image2->SetSpacing(spacing2);

  image1->Allocate();
  image2->Allocate();
  image1->FillBuffer(0);
  image2->FillBuffer(0);

  // Create box 1 in image1: indices (10,10,10) to (29,29,29)
  for (int z = 10; z < 30; ++z) {
    for (int y = 10; y < 30; ++y) {
      for (int x = 10; x < 30; ++x) {
        ImageType::IndexType idx;
        idx[0] = x; idx[1] = y; idx[2] = z;
        image1->SetPixel(idx, 255);
      }
    }
  }

  // Create box 2 in image2: indices (10,10,35) to (29,29,44)
  for (int z = 35; z < 45; ++z) {
    for (int y = 10; y < 30; ++y) {
      for (int x = 10; x < 30; ++x) {
        ImageType::IndexType idx;
        idx[0] = x; idx[1] = y; idx[2] = z;
        image2->SetPixel(idx, 255);
      }
    }
  }

  std::cout << "\n=== Test: Different spacing on two input images ===" << std::endl;
  std::cout << "Image1 spacing: " << image1->GetSpacing() << std::endl;
  std::cout << "Image2 spacing: " << image2->GetSpacing() << std::endl;

  using FilterType = itk::HausdorffDistanceImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->SetUseImageSpacing(true);
  
  std::cout << "\nRunning HausdorffDistanceImageFilter..." << std::endl;
  try {
    filter->Update();
    const FilterType::RealType distance = filter->GetHausdorffDistance();
    std::cout << "Hausdorff distance: " << distance << std::endl;
    
    // This configuration doesn't make physical sense because the two images
    // represent different physical geometries (image2's box is stretched in z).
    // The filter should either:
    // 1. Check that spacing matches and throw an exception
    // 2. Handle it correctly by transforming coordinates
    
    std::cout << "\nWARNING: The filter accepts images with different spacing," << std::endl;
    std::cout << "but this may lead to incorrect results because the same index" << std::endl;
    std::cout << "represents different physical locations in the two images." << std::endl;
  }
  catch (const itk::ExceptionObject & ex) {
    std::cout << "Filter threw exception (this might be expected): " << ex << std::endl;
  }

  return EXIT_SUCCESS;
}
