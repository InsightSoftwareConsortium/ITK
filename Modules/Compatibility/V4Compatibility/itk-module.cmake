set(DOCUMENTATION "Classes that provide ITKv4 compatibility. The CMake
variable ITKV4_COMPATIBILITY must be ON to use these classes.")

if(NOT ITKV4_COMPATIBILITY)
  set(EXCLUDE_V4COMPATIBILITY "EXCLUDE_FROM_DEFAULT")
else()
  set(EXCLUDE_V4COMPATIBILITY "")
endif()
itk_module(ITKV4Compatibility
  COMPILE_DEPENDS
    ITKDisplacementField
  TEST_DEPENDS
    ITKTestKernel
  ${EXCLUDE_V4COMPATIBILITY}
  DESCRIPTION
    "${DOCUMENTATION}"
)
