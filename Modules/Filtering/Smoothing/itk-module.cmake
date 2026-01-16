set(
  DOCUMENTATION
  "This module includes the most common image smoothing
filters. For example, Gaussian and Median filters. You may also find it
interesting to look at the \\\\ref ITKAnisotropicSmoothing group of filters."
)

itk_module(
  ITKSmoothing
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKConvolution
    ITKImageSources
  COMPILE_DEPENDS
    ITKFFT
    ITKImageFunction
  TEST_DEPENDS
    ITKConvolution
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
)
