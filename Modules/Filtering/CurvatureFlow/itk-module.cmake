set(DOCUMENTATION "This module contains filters that implement variations of
Curvature Flow. This is a technique that uses an iterative solution of partial
differential equations to implement image denoising image filtering. These
classes are typically used as edge-preserving smoothing filters. You may also
find the \\\\ref ITKSmoothing and the \\\\ref ITKAnisotropicSmoothing useful as
well.")

itk_module(ITKCurvatureFlow
  COMPILE_DEPENDS
    ITKFiniteDifference
    ITKImageFilterBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
