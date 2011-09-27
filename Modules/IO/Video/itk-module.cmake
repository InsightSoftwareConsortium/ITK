set(DOCUMENTATION "This module contains the basic framework for Video IO as
well as the FileListIO mechanism that does not depend on any outside
libraries.")

itk_module(ITKIOVideo
  DEPENDS
    ITKIOBase
    ITKVideoCore
  TEST_DEPENDS
    ITKVideoCore
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
