set(DOCUMENTATION "This module contains the GPU implementation of filters that
implement variations of anisotropic smoothing. This is an image denoising
technique that strives for preserving edges on the images while smoothing regions
of uniform intensity.  This type of filtering is convenient as a preprocessing
stage of segmentation algorithms. You may find useful as well the filters in the
ITKGPUSmoothingModule.")

itk_module(ITKGPUAnisotropicSmoothing
  DEPENDS
    ITKCommon
    ITKGPUCommon
    ITKGPUFiniteDifference
  COMPILE_DEPENDS
    ITKAnisotropicSmoothing
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
