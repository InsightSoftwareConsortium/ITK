set(
  DOCUMENTATION
  "http://www.insight-journal.org/browse/publication/213
"
)

itk_module(
  MinimalPathExtraction
  DEPENDS
    ITKCommon
    ITKOptimizers
    ITKPath
    ITKFastMarching
  TEST_DEPENDS
    ITKTestKernel
    ITKIOSpatialObjects
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
