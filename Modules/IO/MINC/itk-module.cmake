set(DOCUMENTATION "This module contains classes for reading and writing image
files in the MINC 2 file Format (mnc) and transformations (xfm).")

itk_module(ITKIOMINC
  ENABLE_SHARED
  DEPENDS
    ITKMINC
    ITKIOImageBase
    ITKIOTransformBase
  COMPILE_DEPENDS
    ITKTransform
  TEST_DEPENDS
    ITKTestKernel
    ITKImageStatistics
    ITKDisplacementField
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
