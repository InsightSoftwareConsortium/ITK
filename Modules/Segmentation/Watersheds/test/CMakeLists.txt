itk_module_test()
set(ITKWatershedsTests
itkTobogganImageFilterTest.cxx
itkIsolatedWatershedImageFilterTest.cxx
itkWatershedImageFilterTest.cxx
)

CreateTestDriver(ITKWatersheds  "${ITKWatersheds-Test_LIBRARIES}" "${ITKWatershedsTests}")

itk_add_test(NAME itkTobogganImageFilterTest
      COMMAND ITKWatershedsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/TobogganImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png
    itkTobogganImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png)
itk_add_test(NAME itkIsolatedWatershedImageFilterTest
      COMMAND ITKWatershedsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/IsolatedWatershedImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
    itkIsolatedWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png 113 84 120 99)
itk_add_test(NAME itkWatershedImageFilterTest
      COMMAND ITKWatershedsTestDriver itkWatershedImageFilterTest)
