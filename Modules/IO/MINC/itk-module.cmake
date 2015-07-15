set(DOCUMENTATION "This module contains classes for reading and writing image
files in the MINC 2 file Format (mnc) and transformations (xfm).")

itk_module(ITKIOMINC
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
    ITKMINC
    ITKIOTransformBase
  TEST_DEPENDS
    ITKTestKernel
    ITKImageStatistics
    ITKDisplacementField
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
