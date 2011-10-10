set(DOCUMENTATION "This module contains denoising algorithms such as patch-based denoising.")

itk_module(ITKDenoising
  DEPENDS
    ITKCommon
    ITKStatistics
    ITKImageStatistics
    ITKImageGrid
    ITKTransform
    ITKImageFunction
    ITKImageAdaptors
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
