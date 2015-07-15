set(DOCUMENTATION "This module contains classes for reading and writing XML
files with the expat library.")

itk_module(ITKIOXML
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKExpat
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
