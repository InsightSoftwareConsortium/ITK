# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in TotalVariation
# TotalVariation depends on ITKCommon
# The testing module in TotalVariation depends on ITKTestKernel
# and ITKMetaIO(besides TotalVariation and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  TotalVariation
  DEPENDS
    ITKCommon
    ITKImageFilterBase
    ITKEigen3
  COMPILE_DEPENDS
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
