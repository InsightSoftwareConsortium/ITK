set(
  DOCUMENTATION
  "This module contains classes for mathematical morphology using parabolic functions.
Parabolic functions can be used to build fast distance transforms, and
binary morphology using spheres.
  http://www.insight-journal.org/browse/publication/228
  http://hdl.handle.net/1926/1370"
)
itk_module(
  ParabolicMorphology
  DEPENDS
    ITKIOImageBase
    ITKThresholding
  TEST_DEPENDS
    ITKImageGrid
    ITKTestKernel
    ITKMathematicalMorphology
    ITKSmoothing
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
