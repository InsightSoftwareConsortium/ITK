set(DOCUMENTATION "This module groups image filters whose operations are
related to manipulations of the underlying image grid. For example, flipping an
image, permuting axis, padding, cropping, pasting, tiling, resampling,
shrinking, and changing its origin or spacing or orientation.")

itk_module(ITK-ImageGrid
  DEPENDS
    ITK-ImageFunction
    ITK-ImageFilterBase
  TEST_DEPENDS
    ITK-TestKernel
    ITK-RegistrationCommon
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on ITK-RegistrationCommon is introduced by itkShrinkImagePreserveObjectPhysicalLocations.
