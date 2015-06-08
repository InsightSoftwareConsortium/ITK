set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in  Matlab format.")

itk_module(ITKIOTransformMatlab
  DEPENDS
    ITKIOTransformBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
