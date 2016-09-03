set(DOCUMENTATION "This module contains a factory to create transforms
from a string identifier.")

itk_module(ITKTransformFactory
  DEPENDS
    ITKCommon
    ITKTransform
  COMPILE_DEPENDS
    ITKDisplacementField
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
