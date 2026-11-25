itk_module(
  RLEImage
  ENABLE_SHARED
  DEPENDS
    ITKImageGrid
  TEST_DEPENDS
    ITKTestKernel
    ITKGoogleTest
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "Run-length encoded memory compression scheme for an itk::Image."
)
