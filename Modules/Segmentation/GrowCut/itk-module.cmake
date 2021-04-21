# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in GrowCut
# GrowCut depends on ITKCommon
# The testing module in GrowCut depends on ITKTestKernel
# and ITKMetaIO(besides GrowCut and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  GrowCut
  DEPENDS
    ITKCommon
    ITKImageGrid
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
