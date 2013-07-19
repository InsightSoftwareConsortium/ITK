set(DOCUMENTATION "This module contains a class for reading the SiemensVision
medical file format.")

itk_module(ITKIOSiemens
  ENABLE_SHARED
  DEPENDS
    ITKIOIPL
    ITKIOGE
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
