set(DOCUMENTATION "This module contains classes to perform energy-based region
growing for multiband images.  Since this is based on G. Koepfler, C. Lopez and
J. M. Morel's work, the acronym KLM is added to quality the region growing
method.  See also \\\\ref ITKRegionGrowing.")

itk_module(ITKKLMRegionGrowing
  ENABLE_SHARED
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKStatistics
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on ITKStatistics in introduced by itkRegionGrow2DTest.
