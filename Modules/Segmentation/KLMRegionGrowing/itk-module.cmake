itk_module(ITKKLMRegionGrowing
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKStatistics
)

# Extra test dependency on ITKStatistics in introduced by itkRegionGrow2DTest.
