set(DOCUMENTATION "This module contains the  classes for handling
various itkTransfrom IO formats.")

itk_module(ITKIOTransform
  DEPENDS
    ITKTransform
    ITKHDF5
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
