set(DOCUMENTATION "This module contains classes implementing bias correction
methods. This is commonly needed for MRI imaging when the intensities in the
middle of the image are higher than the intensities in the borders of the
image. The same need is common in microscopy images when the illumination is not
uniform across the field of view.")

itk_module(ITKBiasCorrection
  ENABLE_SHARED
  DEPENDS
    ITKImageFilterBase
    ITKImageGrid
    ITKImageIntensity
    ITKPolynomials
    ITKStatistics
    ITKOptimizers
  TEST_DEPENDS
    ITKTestKernel
    ITKImageSources
    ITKImageIntensity
    ITKImageGrid
    ITKThresholding
  DESCRIPTION
    "${DOCUMENTATION}"
)
