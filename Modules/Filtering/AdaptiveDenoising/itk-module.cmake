# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in AdaptiveDenoising
# AdaptiveDenoising depends on ITKCommon
# The testing module in AdaptiveDenoising depends on ITKTestKernel
# and ITKMetaIO(besides AdaptiveDenoising and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  AdaptiveDenoising
  DEPENDS
    ITKCommon
    ITKSmoothing
    ITKStatistics
    ITKImageStatistics
  COMPILE_DEPENDS
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
