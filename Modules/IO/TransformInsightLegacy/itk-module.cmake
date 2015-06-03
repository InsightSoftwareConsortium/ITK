set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in txt format.")

itk_module(ITKIOTransformInsightLegacy
  ENABLE_SHARED
  DEPENDS
    ITKIOTransformBase
    ITKDoubleConversion
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
