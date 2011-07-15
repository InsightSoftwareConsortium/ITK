set(DOCUMENTATION "This module groups image filters whose operations are
related to manipulations of the underlying image grid. For example, flipping an
image, permuting axis, padding, cropping, pasting, tiling, resampling,
shrinking, and changing its origin or spacing or orientation.")

itk_module(ITKImageGrid
  DEPENDS
    ITKImageFunction
    ITKImageFilterBase
  TEST_DEPENDS
    ITKTestKernel
    ITKRegistrationCommon
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on ITKRegistrationCommon is introduced by itkShrinkImagePreserveObjectPhysicalLocations.
