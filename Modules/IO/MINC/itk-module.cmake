set(DOCUMENTATION "This module contains classes for reading and writing image
files in the MINC 2 file Format (mnc) and transformations (xfm).")

itk_module(ITKIOMINC
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKMINC
  TEST_DEPENDS
    ITKTestKernel
    ITKMINC
    ITKImageStatistics
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
