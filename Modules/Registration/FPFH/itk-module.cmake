# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in Fpfh
# Fpfh depends on ITKCommon
# The testing module in Fpfh depends on ITKTestKernel
# and ITKMetaIO(besides Fpfh and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  Fpfh
  DEPENDS
    ITKCommon
  COMPILE_DEPENDS
    ITKRegistrationCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
    ITKIOMeshBase
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
