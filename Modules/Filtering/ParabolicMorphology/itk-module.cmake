set(
  DOCUMENTATION
  "This module contains classes for mathematical morphology using parabolic functions.
Parabolic functions can be used to build fast distance transforms, and
binary morphology using spheres."
)
itk_module(
  ParabolicMorphology
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKFiltering
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
