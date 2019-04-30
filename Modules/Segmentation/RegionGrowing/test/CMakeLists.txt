itk_module_test()
set(ITKRegionGrowingTests
itkNeighborhoodConnectedImageFilterTest.cxx
itkIsolatedConnectedImageFilterTest.cxx
itkConfidenceConnectedImageFilterTest.cxx
itkVectorConfidenceConnectedImageFilterTest.cxx
itkConnectedThresholdImageFilterTest.cxx
)

CreateTestDriver(ITKRegionGrowing  "${ITKRegionGrowing-Test_LIBRARIES}" "${ITKRegionGrowingTests}")

itk_add_test(NAME itkNeighborhoodConnectedImageFilterTest
      COMMAND ITKRegionGrowingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/NeighborhoodConnectedImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
    itkNeighborhoodConnectedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png 146 167)
itk_add_test(NAME itkIsolatedConnectedImageFilterTest
      COMMAND ITKRegionGrowingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
    itkIsolatedConnectedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png true 115 85 107 110)
itk_add_test(NAME itkIsolatedConnectedImageFilterTest2
      COMMAND ITKRegionGrowingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
    itkIsolatedConnectedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png false 175 125 100 170 176 125 101 170)
itk_add_test(NAME itkConfidenceConnectedImageFilterTest
      COMMAND ITKRegionGrowingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ConfidenceConnectedImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
    itkConfidenceConnectedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png 165 165)
itk_add_test(NAME itkConfidenceConnectedImageFilterTest2
      COMMAND ITKRegionGrowingTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest2.png fcd6bcb56c1689fcef28b57c22475bad
    itkConfidenceConnectedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest2.png 9999 9999)
itk_add_test(NAME itkVectorConfidenceConnectedImageFilterTest
      COMMAND ITKRegionGrowingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConfidenceConnectedImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
    itkVectorConfidenceConnectedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png 72 44 23 67 5.0 6)
itk_add_test(NAME itkVectorConfidenceConnectedImageFilterTest2
      COMMAND ITKRegionGrowingTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest2.png 06aa8c7e4e2aeb3a3028d5b9d5fe6e27
    itkVectorConfidenceConnectedImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png}
      ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest2.png 9999 9999 9999 9999 5.0 6)
itk_add_test(NAME itkConnectedThresholdImageFilterTest
      COMMAND ITKRegionGrowingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
    itkConnectedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png 165 165 0 150)
itk_add_test(NAME itkConnectedThresholdImageFilterTest2
      COMMAND ITKRegionGrowingTestDriver
  --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest2.png}
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
   itkConnectedThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/8ConnectedImage.bmp}
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
            29 47 200 255 1)
