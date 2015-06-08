set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in HDF5 format.")

itk_module(ITKIOTransformHDF5
  DEPENDS
    ITKIOTransformBase
    ITKHDF5
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
