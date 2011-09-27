itk_module_test()
set(ITKHighDimensionalMetricsTests
  itkObjectToObjectMetricTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKHighDimensionalMetrics  "${ITKHighDimensionalMetrics-Test_LIBRARIES}" "${ITKHighDimensionalMetricsTests}")

itk_add_test(NAME itkObjectToObjectMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver itkObjectToObjectMetricTest)
