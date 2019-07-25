set(DOCUMENTATION "This module contains classes implementing bias correction
methods. This is commonly needed for MRI imaging when the intensities in the
middle of the image are higher than the intensities in the borders of the
image. The same need is common in microscopy images when the illumination is not
uniform across the field of view.")

itk_module(ITKBiasCorrection
  ENABLE_SHARED
  DEPENDS
    ITKImageIntensity
  PRIVATE_DEPENDS
    ITKCommon
  COMPILE_DEPENDS
    ITKOptimizers
    ITKImageGrid
    ITKPolynomials
  TEST_DEPENDS
    ITKTestKernel
    ITKOptimizers
    ITKPolynomials
    ITKThresholding
  DESCRIPTION
    "${DOCUMENTATION}"
)
