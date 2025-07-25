set(
  DOCUMENTATION
  "This module contains filter/functions for converting
grayscale images to colormapped RGB images."
)

itk_module(
  ITKColormap
  ENABLE_SHARED
  COMPILE_DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
)
