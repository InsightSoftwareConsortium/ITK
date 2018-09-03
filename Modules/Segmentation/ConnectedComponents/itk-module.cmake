set(DOCUMENTATION "This module contains modules to identify and modify connected
components. Theses algorithms are commonly applied to binary or label map
images. See also ITKClassifiers, ITKLabelMap, and
ITKBinaryMathematicalMorphology.")

itk_module(ITKConnectedComponents
  DEPENDS
    ITKImageIntensity
    ITKThresholding
    ITKImageGrid
    ITKImageLabel
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
