# the top-level README is used for describing this module, just
# re-used it for documentation here
get_filename_component(MY_CURRENT_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
file(READ "${MY_CURRENT_DIR}/README.rst" DOCUMENTATION)

# define the dependencies of the include module and the tests
itk_module(AnisotropicDiffusionLBR
  DEPENDS
    ITKCommon
    ITKIOImageBase
    ITKIOSpatialObjects
    ITKMetaIO
    ITKImageGradient
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  # Header only library, no ENABLE_SHARED
)
