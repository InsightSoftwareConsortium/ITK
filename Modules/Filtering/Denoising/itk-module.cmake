set(
  DOCUMENTATION
  "This module contains denoising algorithms such as patch-based denoising."
)

itk_module(
  ITKDenoising
  ENABLE_SHARED
  DEPENDS
    ITKImageAdaptors
    ITKImageGrid
    ITKImageStatistics
    ITKIOImageBase
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
    ITKImageGrid
  DESCRIPTION "${DOCUMENTATION}"
)
