# the top-level README is used for describing this module, just
# re-used it for documentation here
itk_module(
  Ransac
  ENABLE_SHARED
  COMPILE_DEPENDS
    ITKCommon
    ITKMesh
    ITKIOMeshBase
    ITKRegistrationCommon
    ITKMetricsv4
    ITKRegistrationMethodsv4
    ITKOptimizersv4
    ITKOptimizers
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
