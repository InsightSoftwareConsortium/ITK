itk_module_test()
set(ITK-RegionGrowingTests
itkNeighborhoodConnectedImageFilterTest.cxx
itkIsolatedConnectedImageFilterTest.cxx
itkRegionGrowingHeaderTest.cxx
)

CreateTestDriver(ITK-RegionGrowing  "${ITK-RegionGrowing-Test_LIBRARIES}" "${ITK-RegionGrowingTests}")

add_test(NAME itkRegionGrowingHeaderTest
      COMMAND ITK-RegionGrowingTestDriver itkRegionGrowingHeaderTest)
add_test(NAME itkNeighborhoodConnectedImageFilterTest
      COMMAND ITK-RegionGrowingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NeighborhoodConnectedImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
    itkNeighborhoodConnectedImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png 146 167)
add_test(NAME itkIsolatedConnectedImageFilterTest
      COMMAND ITK-RegionGrowingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
    itkIsolatedConnectedImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png true 115 85 107 110)
add_test(NAME itkIsolatedConnectedImageFilterTest2
      COMMAND ITK-RegionGrowingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
    itkIsolatedConnectedImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png false 175 125 100 170 176 125 101 170)
