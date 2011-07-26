set(DOCUMENTATION "This module contains classes to perform the region growing
approach to image segmentation.  A seed pixel is iteratively propagated to a
region identifying a tissue type by testing if connected pixels pass a criteria.
See also \\\\ref ITKKLMRegionGrowing.")

itk_module(ITKRegionGrowing
  DEPENDS
    ITKThresholding
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
