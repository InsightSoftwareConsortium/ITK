# Maintainer: Richard Beare <richard.beare@monash.edu>
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
  DESCRIPTION
    "Classes performing morphology using parabolic functions. Fast distance transforms and binary erosions/dilations/openings/closings by spheres, sharpenings and grayscale operations. https://doi.org/10.54294/aq68pt"
)
