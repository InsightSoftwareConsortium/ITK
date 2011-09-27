itk_module_test()
set(ITKKLMRegionGrowingTests
itkRegionGrow2DTest.cxx
)

CreateTestDriver(ITKKLMRegionGrowing  "${ITKKLMRegionGrowing-Test_LIBRARIES}" "${ITKKLMRegionGrowingTests}")

itk_add_test(NAME itkRegionGrow2DTest
      COMMAND ITKKLMRegionGrowingTestDriver itkRegionGrow2DTest)
