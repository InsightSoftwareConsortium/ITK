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
    SplitComponents
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
)
