set(DOCUMENTATION "This module contains classes related to calculation of
eigenvectors and eigenvalues.")

itk_module(ITKEigen
  DEPENDS
    ITKImageFilterBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
