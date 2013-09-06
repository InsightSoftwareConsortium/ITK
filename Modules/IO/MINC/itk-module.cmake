set(DOCUMENTATION "This module contains classes for reading and writing image
files in the MINC 2 file Format (mnc).")

itk_module(ITKIOMINC
  DEPENDS
    ITKMINC
    ITKIOImageBase
    ITKIOTransformBase
    ITKTransform
  TEST_DEPENDS
    ITKTestKernel
    ITKImageStatistics
    ITKDisplacementField
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
