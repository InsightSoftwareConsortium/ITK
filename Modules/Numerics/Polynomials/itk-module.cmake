set(DOCUMENTATION "This module contains code for the calculation of polynomial
functions.")

itk_module(ITKPolynomials
  ENABLE_SHARED
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
