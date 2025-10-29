/*=========================================================================
 *
 *  Definitive test showing ITK correctly handles anisotropic spacing
 *
 *=========================================================================*/

#include "itkHausdorffDistanceImageFilter.h"
#include "itkTestingMacros.h"

int
itkHausdorffDistanceImageFilterAnisotropicCorrectness(int, char *[])
{
  constexpr unsigned int ImageDimension{ 3 };

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  std::cout << "\n=== Definitive Test: ITK Correctly Handles Anisotropic Spacing ===" << std::endl;
  std::cout << "\nScenario: Two points separated in z-direction only" << std::endl;

  int exitStatus = EXIT_SUCCESS;

  // Create simple test: two single voxels
  auto image1 = ImageType::New();
  auto image2 = ImageType::New();

  ImageType::SizeType size;
  size.Fill(20);

  image1->SetRegions(size);
  image2->SetRegions(size);

  // Test with anisotropic spacing (1,1,2)
  ImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  spacing[2] = 2.0;
  image1->SetSpacing(spacing);
  image2->SetSpacing(spacing);

  image1->Allocate();
  image2->Allocate();
  image1->FillBuffer(0);
  image2->FillBuffer(0);

  // Point 1: at voxel index (10,10,5)
  ImageType::IndexType idx1;
  idx1[0] = 10;
  idx1[1] = 10;
  idx1[2] = 5;
  image1->SetPixel(idx1, 255);

  // Point 2: at voxel index (10,10,10)  
  ImageType::IndexType idx2;
  idx2[0] = 10;
  idx2[1] = 10;
  idx2[2] = 10;
  image2->SetPixel(idx2, 255);

  // Calculate expected physical distance
  ImageType::PointType pt1, pt2;
  image1->TransformIndexToPhysicalPoint(idx1, pt1);
  image2->TransformIndexToPhysicalPoint(idx2, pt2);

  std::cout << "\nPoint 1 index: " << idx1 << " -> physical: " << pt1 << std::endl;
  std::cout << "Point 2 index: " << idx2 << " -> physical: " << pt2 << std::endl;

  double expectedDistance = std::sqrt(
    (pt2[0] - pt1[0]) * (pt2[0] - pt1[0]) +
    (pt2[1] - pt1[1]) * (pt2[1] - pt1[1]) +
    (pt2[2] - pt1[2]) * (pt2[2] - pt1[2])
  );

  std::cout << "Expected physical distance: " << expectedDistance << std::endl;
  std::cout << "  (voxel distance in z: 5 voxels * spacing 2.0 = 10.0 physical units)" << std::endl;

  // Compute using ITK
  using FilterType = itk::HausdorffDistanceImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->SetUseImageSpacing(true);
  filter->Update();

  const FilterType::RealType computedDistance = filter->GetHausdorffDistance();
  std::cout << "\nITK computed distance: " << computedDistance << std::endl;

  if (std::abs(computedDistance - expectedDistance) > 0.1)
  {
    std::cerr << "ERROR: Distance mismatch!" << std::endl;
    std::cerr << "Expected: " << expectedDistance << std::endl;
    std::cerr << "Computed: " << computedDistance << std::endl;
    exitStatus = EXIT_FAILURE;
  }
  else
  {
    std::cout << "\n✓ ITK correctly computed the physical distance!" << std::endl;
    std::cout << "✓ Anisotropic spacing (1,1,2) was properly accounted for" << std::endl;
  }

  std::cout << "\n=== Conclusion ===" << std::endl;
  std::cout << "ITK's HausdorffDistanceImageFilter correctly handles anisotropic spacing." << std::endl;
  std::cout << "The distance is computed in physical units, properly accounting for" << std::endl;
  std::cout << "different spacing values in different dimensions." << std::endl;

  return exitStatus;
}
