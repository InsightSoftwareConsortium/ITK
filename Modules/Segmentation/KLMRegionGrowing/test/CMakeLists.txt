itk_module_test()
set(ITKKLMRegionGrowingTests
itkKLMRegionGrowingHeaderTest.cxx
itkRegionGrow2DTest.cxx
)

CreateTestDriver(ITKKLMRegionGrowing  "${ITKKLMRegionGrowing-Test_LIBRARIES}" "${ITKKLMRegionGrowingTests}")

itk_add_test(NAME itkKLMRegionGrowingHeaderTest
      COMMAND ITKKLMRegionGrowingTestDriver itkKLMRegionGrowingHeaderTest)
itk_add_test(NAME itkRegionGrow2DTest
      COMMAND ITKKLMRegionGrowingTestDriver itkRegionGrow2DTest)
