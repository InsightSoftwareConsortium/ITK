set(DOCUMENTATION "This module contains filters that compute differential
operations in images. In particular, image gradients, gradient magnitude and
difference of Gaussians.")

itk_module(ITK-ImageGradient
  DEPENDS
    ITK-ImageIntensity
    ITK-Smoothing
  TEST_DEPENDS
    ITK-TestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
