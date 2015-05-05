set(DOCUMENTATION "This module contains the basic framework for Video IO as
well as the FileListIO mechanism that does not depend on any outside
libraries.")

itk_module(ITKVideoIO
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
    ITKVideoCore
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
