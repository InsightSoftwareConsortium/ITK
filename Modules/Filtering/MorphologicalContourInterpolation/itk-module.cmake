set(
  DOCUMENTATION
  "The modules provides filters to do interpolation
of manually segmented anatomical contours."
)

itk_module(
  MorphologicalContourInterpolation
  DEPENDS
    ITKCommon
    ITKBinaryMathematicalMorphology
    ITKDistanceMap
  TEST_DEPENDS
    ITKIOImageBase # typename
    ITKTestKernel
    RLEImage
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
