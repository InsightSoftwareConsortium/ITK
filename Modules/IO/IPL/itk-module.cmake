set(DOCUMENTATION "This module contains code common to both the GE and Siemens
IO modules.")

itk_module(ITKIOIPL
  DEPENDS
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
