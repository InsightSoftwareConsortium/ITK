set(DOCUMENTATION "This module contains denoising algorithms such as patch-based denoising.")

itk_module(ITKDenoising
  COMPILE_DEPENDS
    ITKImageAdaptors
    ITKImageGrid
    ITKImageStatistics
    ITKIOImageBase
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
    ITKImageGrid
  DESCRIPTION
    "${DOCUMENTATION}"
)
