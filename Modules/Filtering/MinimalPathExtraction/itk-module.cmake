# the top-level README is used for describing this module, just
# re-used it for documentation here
itk_module(
  MinimalPathExtraction
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKOptimizers
    ITKPath
    ITKFastMarching
  TEST_DEPENDS
    ITKTestKernel
    ITKIOSpatialObjects
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
