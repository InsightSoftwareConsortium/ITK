itk_module(ITK-IO-Meta
  DEPENDS
    ITK-MetaIO
    ITK-IO-Base
  TEST_DEPENDS
    ITK-TestKernel
    ITK-Smoothing
)

# Extra test dependency of ITK-Smoothing is caused by itkMetaStreamingIOTest.
