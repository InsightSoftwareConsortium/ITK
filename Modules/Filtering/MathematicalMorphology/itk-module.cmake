set(DOCUMENTATION "This module contains classes that implement variations of
mathematical morphology techniques for grayscale images. In addition to the classical erosion,
dilation, opening and closing filters, you will find here geodesic operations,
maxima and minima filters, and reconstruction filters.")

itk_module(ITKMathematicalMorphology
  COMPILE_DEPENDS
    ITKImageFilterBase
    ITKImageIntensity
    ITKImageGrid
    ITKThresholding
    ITKConnectedComponents
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
