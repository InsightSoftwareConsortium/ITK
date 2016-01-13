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
    ITKIOImageBase # typename
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
