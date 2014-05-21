set(
  DOCUMENTATION
  "This module contains classes for performing phase symmetry
filtration."
)

itk_module(
  PhaseSymmetry
  DEPENDS
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
)
