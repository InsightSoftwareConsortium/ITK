set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in MINC format (.xfm).")

itk_module(ITKIOTransformMINC
  ENABLE_SHARED
  DEPENDS
    ITKIOTransformBase
    ITKMINC
  COMPILE_DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKIOMINC
  TEST_DEPENDS
    ITKTestKernel
    ITKDisplacementField
  FACTORY_NAMES
    TransformIO::MINC
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
