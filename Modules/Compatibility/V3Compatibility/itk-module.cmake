set(DOCUMENTATION "Classes that provide ITKv3 compatibility. The CMake
variable ITKV3_COMPATIBILITY must be ON to use these classes.")

if(NOT ITKV3_COMPATIBILITY)
  set(EXCLUDE_V3COMPATIBILITY "EXCLUDE_FROM_DEFAULT")
else()
  set(EXCLUDE_V3COMPATIBILITY "")
endif()
itk_module(ITKV3Compatibility
  COMPILE_DEPENDS
    ITKDisplacementField
  TEST_DEPENDS
    ITKTestKernel
  ${EXCLUDE_V3COMPATIBILITY}
  DESCRIPTION
    "${DOCUMENTATION}"
)
