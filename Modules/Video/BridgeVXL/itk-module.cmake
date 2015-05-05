set(DOCUMENTATION "This module contains both I/O and bridging methods needed
for interacting with and utilizing VXL within ITK.")

itk_module(ITKVideoBridgeVXL
  ENABLE_SHARED
  DEPENDS
    ITKVideoIO
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "${DOCUMENTATION}"
  )
