# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in IOScanco
# The testing module in IOScanco depends on ITKTestKernel
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  IOScanco
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKIOMeta
  FACTORY_NAMES
    ImageIO::Scanco
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
