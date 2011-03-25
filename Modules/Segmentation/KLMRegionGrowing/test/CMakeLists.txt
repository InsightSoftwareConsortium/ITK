itk_module_test()
set(ITK-KLMRegionGrowingTests
itkKLMRegionGrowingHeaderTest.cxx
itkRegionGrow2DTest.cxx
)

CreateTestDriver(ITK-KLMRegionGrowing  "${ITK-KLMRegionGrowing-Test_LIBRARIES}" "${ITK-KLMRegionGrowingTests}")

add_test(NAME itkKLMRegionGrowingHeaderTest
      COMMAND ITK-KLMRegionGrowingTestDriver itkKLMRegionGrowingHeaderTest)
add_test(NAME itkRegionGrow2DTest
      COMMAND ITK-KLMRegionGrowingTestDriver itkRegionGrow2DTest)
