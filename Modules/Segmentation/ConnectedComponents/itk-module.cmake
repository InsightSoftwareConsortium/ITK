set(DOCUMENTATION "This module contains modules to identify and modify connected
components.  Theses algorithms are commonly applied to binary or label map
images.  See also \\\\ref ITKClassifiers, \\\\ref ITKLabelMap, and \\\\ref
ITKBinaryMathematicalMorphology.")

itk_module(ITKConnectedComponents
  DEPENDS
    ITKImageIntensity
    ITKThresholding
    ITKImageGrid
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
