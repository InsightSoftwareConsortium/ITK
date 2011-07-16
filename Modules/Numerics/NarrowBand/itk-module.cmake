set(DOCUMENTATION "This module contains code for data types and calculation on a
narrow band of space.")

itk_module(ITKNarrowBand
  DEPENDS
    ITKImageIntensity
    ITKFiniteDifference
    ITKCurvatureFlow
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
