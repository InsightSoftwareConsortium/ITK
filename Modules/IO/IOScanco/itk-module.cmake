set(
  DOCUMENTATION
  "This module contains IO classes for reading and writing
Scanco Medical microCT images. These images are usually saved as .isq or .aim
files."
)

itk_module(
  IOScanco
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
)
