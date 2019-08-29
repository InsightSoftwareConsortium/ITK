set(DOCUMENTATION "This module contains filters that compute differential
operations in images. In particular, image gradients, gradient magnitude and
difference of Gaussians.")

itk_module(ITKImageGradient
  DEPENDS
    ITKSmoothing
    ITKImageIntensity
  COMPILE_DEPENDS
    ITKImageAdaptors
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
