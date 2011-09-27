set(DOCUMENTATION "This module contains classes that implement variations of
mathematical morphology techniques for grayscale images. In addition to the classical erosion,
dilation, opening and closing filters, you will find here geodesic operations,
maxima and minima filters, and reconstruction filters.")

itk_module(ITKMathematicalMorphology
  DEPENDS
    ITKImageIntensity
    ITKImageGrid
    ITKConnectedComponents
  TEST_DEPENDS
    ITKTestKernel
    ITKIOPNG
  DESCRIPTION
    "${DOCUMENTATION}"
)
