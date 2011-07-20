set(DOCUMENTATION "This module contains classes for reading and writing image
files in the \"legacy\" (non-XML) VTK file format.")

itk_module(ITKIOVTK
  DEPENDS
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
