set(DOCUMENTATION "This module contains a group of basic video filtering classes.")

itk_module(ITKVideoFiltering
  DEPENDS
    ITKVideoCore
  TEST_DEPENDS
    ITKTestKernel
    ITKVideoIO
    ITKSmoothing
  DESCRIPTION
    "${DOCUMENTATION}"
)
