set(DOCUMENTATION "This module contains filters that compute differential
operations in images. In particular, image gradients, gradient magnitude and
difference of Gaussians.")

itk_module(ITKImageGradient
  COMPILE_DEPENDS
    ITKImageAdaptors
    ITKImageIntensity
    ITKSmoothing
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
