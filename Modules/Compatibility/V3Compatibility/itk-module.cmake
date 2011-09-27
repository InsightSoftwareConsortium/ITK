set(DOCUMENTATION "Classes that provide ITKv3 compatibility. The CMake
variable ITKV3_COMPATIBILITY must be ON to use these classes.")

if(NOT ITKV3_COMPATIBILITY)
  set(EXCLUDE_IT "EXCLUDE_FROM_ALL")
else()
  set(EXCLUDE_IT "")
endif()
itk_module(ITKV3Compatibility
  DEPENDS
    ITKDisplacementField
  TEST_DEPENDS
    ITKTestKernel
  ${EXCLUDE_IT}
  DESCRIPTION
    "${DOCUMENTATION}"
)
