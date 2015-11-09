set(DOCUMENTATION "This module contains code common to both the GE and Siemens
IO modules.")

itk_module(ITKIOIPL
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
