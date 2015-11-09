set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in HDF5 format.")

itk_module(ITKIOTransformHDF5
  PRIVATE_DEPENDS
    ITKIOTransformBase
    ITKHDF5
  TEST_DEPENDS
    ITKTestKernel
    ITKIOTransformBase
    ITKHDF5
  DESCRIPTION
    "${DOCUMENTATION}"
)
