itk_module_test()
set(ITKKLMRegionGrowingTests
itkRegionGrow2DTest.cxx
)

CreateTestDriver(ITKKLMRegionGrowing  "${ITKKLMRegionGrowing-Test_LIBRARIES}" "${ITKKLMRegionGrowingTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkRegionGrow2DTest
      COMMAND ITKKLMRegionGrowingTestDriver
              --redirectOutput ${TEMP}/itkRegionGrow2DTest.txt
      itkRegionGrow2DTest)
set_tests_properties(itkRegionGrow2DTest PROPERTIES
      ATTACHED_FILES_ON_FAIL ${TEMP}/itkRegionGrow2DTest.txt)
