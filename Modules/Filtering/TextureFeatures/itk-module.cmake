# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in ModuleTemplate
# ModuleTemplate depends on ITKCommon
# The testing module in ModuleTemplate depends on ITKTestKernel
# and ITKMetaIO(besides ModuleTemplate and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  TextureFeatures
  DEPENDS
    ITKCommon
    ITKStatistics
    ITKImageGrid
  COMPILE_DEPENDS
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
)
