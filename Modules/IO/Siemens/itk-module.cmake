set(DOCUMENTATION "This module contains a class for reading the SiemensVision
medical file format.")

itk_module(ITKIOSiemens
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKIOIPL
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
