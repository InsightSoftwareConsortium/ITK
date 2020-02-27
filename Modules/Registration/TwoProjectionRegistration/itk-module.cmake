# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in TwoProjectionRegistration
# The testing module in TwoProjectionRegistration depends on ITKTestKernel
# By convention those modules outside of ITK are not prefixed with
# ITK

# define the dependencies of the include module and the tests
itk_module(
  TwoProjectionRegistration
  DEPENDS
    ITKImageFunction
    ITKImageGradient
    ITKOptimizers
    ITKRegistrationCommon
    ITKSpatialObjects
    ITKTransform
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
