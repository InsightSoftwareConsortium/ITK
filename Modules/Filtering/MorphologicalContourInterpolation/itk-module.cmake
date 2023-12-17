set(
  DOCUMENTATION
  "The modules provides filters to do interpolation
of manually segmented anatomical contours.
Enabling testing requires RLEImage module to be enabled."
)

itk_module(
  MorphologicalContourInterpolation
  ENABLE_SHARED
  DEPENDS
    ITKBinaryMathematicalMorphology
    ITKDistanceMap
  TEST_DEPENDS
    ITKTestKernel
    RLEImage
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
