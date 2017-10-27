set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in txt format.")

itk_module(ITKIOTransformInsightLegacy
  ENABLE_SHARED
  DEPENDS
    ITKIOTransformBase
  PRIVATE_DEPENDS
    ITKDoubleConversion
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    TransformIO::Txt
  DESCRIPTION
    "${DOCUMENTATION}"
)
