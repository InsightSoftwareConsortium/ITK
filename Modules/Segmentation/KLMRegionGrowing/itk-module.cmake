itk_module(ITK-KLMRegionGrowing
  DEPENDS
    ITK-Common
  TEST_DEPENDS
    ITK-TestKernel
    ITK-Statistics
)

# Extra test dependency on ITK-Statistics in introduced by itkRegionGrow2DTest.
