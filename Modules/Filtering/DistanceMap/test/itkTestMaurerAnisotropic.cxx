/*=========================================================================
 *
 *  Test SignedMaurerDistanceMapImageFilter with anisotropic spacing
 *
 *=========================================================================*/

#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

int
itkTestMaurerAnisotropic(int, char *[])
{
  using ImageType = itk::Image<unsigned char, 3>;
  using DistanceImageType = itk::Image<float, 3>;
  
  auto image = ImageType::New();
  ImageType::SizeType size;
  size.Fill(50);
  image->SetRegions(size);
  
  // Test with anisotropic spacing
  ImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  spacing[2] = 2.0;
  image->SetSpacing(spacing);
  
  image->Allocate();
  image->FillBuffer(0);
  
  // Set one voxel at (25,25,25)
  ImageType::IndexType idx;
  idx[0] = 25;
  idx[1] = 25;
  idx[2] = 25;
  image->SetPixel(idx, 255);
  
  using FilterType = itk::SignedMaurerDistanceMapImageFilter<ImageType, DistanceImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetUseImageSpacing(true);
  filter->SetSquaredDistance(false);
  filter->Update();
  
  auto distMap = filter->GetOutput();
  
  int exitStatus = EXIT_SUCCESS;
  
  // Check distance at a few points
  ImageType::IndexType testIdx;
  
  // Point at (25,25,20) - 5 voxels away in z, physical distance should be 10
  testIdx[0] = 25;
  testIdx[1] = 25;
  testIdx[2] = 20;
  float dist1 = distMap->GetPixel(testIdx);
  std::cout << "Distance at (25,25,20): " << dist1 << " (expected: 10)" << std::endl;
  if (std::abs(dist1 - 10.0f) > 0.1f) {
    std::cerr << "ERROR: Distance mismatch!" << std::endl;
    exitStatus = EXIT_FAILURE;
  }
  
  // Point at (30,25,25) - 5 voxels away in x, physical distance should be 5
  testIdx[0] = 30;
  testIdx[1] = 25;
  testIdx[2] = 25;
  float dist2 = distMap->GetPixel(testIdx);
  std::cout << "Distance at (30,25,25): " << dist2 << " (expected: 5)" << std::endl;
  if (std::abs(dist2 - 5.0f) > 0.1f) {
    std::cerr << "ERROR: Distance mismatch!" << std::endl;
    exitStatus = EXIT_FAILURE;
  }
  
  // Point at (30,30,20) - 5 away in x, 5 in y, 5 in z
  // Physical: 5 in x, 5 in y, 10 in z -> sqrt(25+25+100) = sqrt(150) ≈ 12.25
  testIdx[0] = 30;
  testIdx[1] = 30;
  testIdx[2] = 20;
  float dist3 = distMap->GetPixel(testIdx);
  float expected3 = std::sqrt(150.0f);
  std::cout << "Distance at (30,30,20): " << dist3 << " (expected: " << expected3 << ")" << std::endl;
  if (std::abs(dist3 - expected3) > 0.1f) {
    std::cerr << "ERROR: Distance mismatch!" << std::endl;
    exitStatus = EXIT_FAILURE;
  }
  
  return exitStatus;
}
