set(DOCUMENTATION "This module contains filters that convolve an image
with a kernel. Convolution is a fundamental operation in many image
analysis algorithms.")

itk_module(ITKConvolution
  ENABLE_SHARED
  DEPENDS
    ITKFFT
    ITKImageIntensity
    ITKThresholding
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
