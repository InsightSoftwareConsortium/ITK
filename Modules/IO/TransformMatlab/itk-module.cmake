set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in  Matlab format.")

itk_module(ITKIOTransformMatlab
  ENABLE_SHARED
  DEPENDS
    ITKIOTransformBase
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    TransformIO::Matlab
  DESCRIPTION
    "${DOCUMENTATION}"
)
