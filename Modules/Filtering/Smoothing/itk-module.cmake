set(DOCUMENTATION "This module includes the most common image smoothing
filters. For example, Gaussian and Median filters. You may also find it
interesting to look at the ITKAnisotropicSmoothing group of filters.")

itk_module(ITKSmoothing
  COMPILE_DEPENDS
    ITKImageFunction
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
