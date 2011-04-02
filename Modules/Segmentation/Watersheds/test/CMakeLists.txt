itk_module_test()
set(ITK-WatershedsTests
itkTobogganImageFilterTest.cxx
itkWatershedsHeaderTest.cxx
itkIsolatedWatershedImageFilterTest.cxx
itkWatershedImageFilterTest.cxx
)

CreateTestDriver(ITK-Watersheds  "${ITK-Watersheds-Test_LIBRARIES}" "${ITK-WatershedsTests}")

add_test(NAME itkWatershedsHeaderTest
      COMMAND ITK-WatershedsTestDriver itkWatershedsHeaderTest)
add_test(NAME itkTobogganImageFilterTest
      COMMAND ITK-WatershedsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TobogganImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png
    itkTobogganImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png)
add_test(NAME itkIsolatedWatershedImageFilterTest
      COMMAND ITK-WatershedsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/IsolatedWatershedImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
    itkIsolatedWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png 113 84 120 99)
add_test(NAME itkWatershedImageFilterTest
      COMMAND ITK-WatershedsTestDriver itkWatershedImageFilterTest)
