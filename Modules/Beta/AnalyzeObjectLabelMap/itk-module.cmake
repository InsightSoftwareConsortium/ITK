# the top-level README is used for describing this module, just
# re-used it for documentation here
get_filename_component(MY_CURRENT_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
file(READ "${MY_CURRENT_DIR}/README.md" DOCUMENTATION)

# define the dependencies of the include module and the tests
itk_module(AnalyzeObjectLabelMap
  DEPENDS
    ITKThresholding
    ITKIOImageBase
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
  FACTORY_NAMES
    ImageIO::AnalyzeObjectLabelMap
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
