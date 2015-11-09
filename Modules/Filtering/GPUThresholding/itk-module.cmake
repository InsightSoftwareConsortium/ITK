set(DOCUMENTATION "This module contains the GPU implementation for image
thresholding filters such as the classical binary thresholding.")

itk_module(ITKGPUThresholding
  DEPENDS
    ITKCommon
    ITKGPUCommon
  COMPILE_DEPENDS
    ITKThresholding
  TEST_DEPENDS
    ITKTestKernel
    ITKGPUSmoothing
  DESCRIPTION
    "${DOCUMENTATION}"
)
