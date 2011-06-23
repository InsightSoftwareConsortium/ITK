itk_module(ITK-Statistics
  DEPENDS
    ITK-Common
    ITK-Netlib
  TEST_DEPENDS
    ITK-TestKernel
    ITK-ImageIntensity
    ITK-ImageCompose
)

# Extra test dependency of ImageIntensity is introduced by itkImageToListSampleAdaptorTest.cxx.
