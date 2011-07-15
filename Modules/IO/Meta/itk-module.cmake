itk_module(ITKIOMeta
  DEPENDS
    ITKMetaIO
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
    ITKSmoothing
)

# Extra test dependency of ITKSmoothing is caused by itkMetaStreamingIOTest.
