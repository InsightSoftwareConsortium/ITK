itk_module_test()
set(ITK-WatershedsTests
itkTobogganImageFilterTest.cxx
itkWatershedsHeaderTest.cxx
)

CreateTestDriver(ITK-Watersheds  "${ITK-Watersheds-Test_LIBRARIES}" "${ITK-WatershedsTests}")

add_test(NAME itkWatershedsHeaderTest
      COMMAND ITK-WatershedsTestDriver itkWatershedsHeaderTest)
add_test(NAME itkTobogganImageFilterTest
      COMMAND ITK-WatershedsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TobogganImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png
    itkTobogganImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png)
