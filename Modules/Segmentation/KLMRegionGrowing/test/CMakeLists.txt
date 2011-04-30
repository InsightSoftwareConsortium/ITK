itk_module_test()
set(ITK-KLMRegionGrowingTests
itkKLMRegionGrowingHeaderTest.cxx
itkRegionGrow2DTest.cxx
)

CreateTestDriver(ITK-KLMRegionGrowing  "${ITK-KLMRegionGrowing-Test_LIBRARIES}" "${ITK-KLMRegionGrowingTests}")

itk_add_test(NAME itkKLMRegionGrowingHeaderTest
      COMMAND ITK-KLMRegionGrowingTestDriver itkKLMRegionGrowingHeaderTest)
itk_add_test(NAME itkRegionGrow2DTest
      COMMAND ITK-KLMRegionGrowingTestDriver itkRegionGrow2DTest)
