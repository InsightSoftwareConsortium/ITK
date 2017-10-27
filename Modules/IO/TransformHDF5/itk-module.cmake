set(DOCUMENTATION "This module contains the classes for the input and output
of itkTransform object in HDF5 format.")

itk_module(ITKIOTransformHDF5
  ENABLE_SHARED
  DEPENDS
    ITKIOTransformBase
  PRIVATE_DEPENDS
    ITKHDF5
  TEST_DEPENDS
    ITKTestKernel
    ITKHDF5
  FACTORY_NAMES
    TransformIO::HDF5
  DESCRIPTION
    "${DOCUMENTATION}"
)
