# the top-level README is used for describing this module, just
# re-used it for documentation here
get_filename_component(MY_CURRENT_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
file(READ "${MY_CURRENT_DIR}/README.md" DOCUMENTATION)

# itk_module() defines the module dependencies in PolarTransform
# PolarTransform depends on ITKCommon
# The testing module in PolarTransform depends on ITKTestKernel
# and ITKMetaIO(besides PolarTransform and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  PolarTransform
  COMPILE_DEPENDS
    ITKTransform
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
