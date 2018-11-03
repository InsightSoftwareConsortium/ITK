set(
  DOCUMENTATION
  "This module contains a filter to compute the strain tensor
field from a displacement field image."
)

itk_module(
  Strain
  DEPENDS
    ITKCommon
    ITKImageGradient
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKIOVTK
    ITKDisplacementField
    ITKIOVTK
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
