set(
  DOCUMENTATION
  "http://www.insight-journal.org/browse/publication/213
"
)

itk_module(
  MinimalPathExtraction
  DEPENDS
    ITKCommon
    ITKPath
    ITKFastMarching
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
