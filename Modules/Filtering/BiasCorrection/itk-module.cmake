set(DOCUMENTATION "This module contains classes implementing bias correction
methods. This is commonly needed for MRI imaging when the intensities in the
middle of the image are higher than the intensities in the borders of the
image. The same need is common in microscopy images when the ilumination is not
uniform accross the field of view.")

itk_module(ITKBiasCorrection
  DEPENDS
    ITKImageFilterBase
    ITKPolynomials
    ITKStatistics
    ITKOptimizers
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
