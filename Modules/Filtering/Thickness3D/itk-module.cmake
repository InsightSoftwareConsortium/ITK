# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in Thickness3D
# Thickness3D depends on ITKCommon
# The testing module in Thickness3D depends on ITKTestKernel
# and ITKMetaIO(besides Thickness3D and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  Thickness3D
  DEPENDS
    ITKCommon
    ITKDistanceMap
    ITKImageIntensity
    ITKImageFilterBase
  COMPILE_DEPENDS
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
