itk_module_test()
set(ITK-ThresholdingTests
itkOtsuThresholdImageCalculatorTest.cxx
itkOtsuMultipleThresholdsImageFilterTest.cxx
itkOtsuThresholdImageFilterTest.cxx
itkThresholdingHeaderTest.cxx
itkThresholdLabelerImageFilterTest.cxx
itkBinaryThresholdImageFilterTest.cxx
itkThresholdImageFilterTest.cxx
itkOtsuMultipleThresholdsCalculatorTest.cxx
itkBinaryThresholdImageFilterTest2.cxx
itkBinaryThresholdProjectionImageFilterTest.cxx
itkBinaryThresholdSpatialFunctionTest.cxx
)

CreateTestDriver(ITK-Thresholding  "${ITK-Thresholding-Test_LIBRARIES}" "${ITK-ThresholdingTests}")

add_test(NAME itkThresholdingHeaderTest
      COMMAND ITK-ThresholdingTestDriver itkThresholdingHeaderTest)
add_test(NAME itkOtsuThresholdImageCalculatorTest
      COMMAND ITK-ThresholdingTestDriver itkOtsuThresholdImageCalculatorTest)
add_test(NAME itkOtsuMultipleThresholdsImageFilterTest
      COMMAND ITK-ThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
    itkOtsuMultipleThresholdsImageFilterTest ${ITK_DATA_ROOT}/Input/peppers.png ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png 64 3 2)
add_test(NAME itkOtsuThresholdImageFilterTest
      COMMAND ITK-ThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuThresholdImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
    itkOtsuThresholdImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png 64)
add_test(NAME itkThresholdLabelerImageFilterTest
      COMMAND ITK-ThresholdingTestDriver itkThresholdLabelerImageFilterTest)
add_test(NAME itkBinaryThresholdImageFilterTest
      COMMAND ITK-ThresholdingTestDriver itkBinaryThresholdImageFilterTest)
add_test(NAME itkThresholdImageFilterTest
      COMMAND ITK-ThresholdingTestDriver itkThresholdImageFilterTest)
add_test(NAME itkOtsuMultipleThresholdsCalculatorTest
      COMMAND ITK-ThresholdingTestDriver itkOtsuMultipleThresholdsCalculatorTest)
add_test(NAME itkBinaryThresholdImageFilterTest2
      COMMAND ITK-ThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png
    itkBinaryThresholdImageFilterTest2 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png)
add_test(NAME itkBinaryThresholdProjectionImageFilterTest
      COMMAND ITK-ThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryThresholdProjection.png
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png
    itkBinaryThresholdProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png 100 255 0)
add_test(NAME itkBinaryThresholdSpatialFunctionTest
      COMMAND ITK-ThresholdingTestDriver itkBinaryThresholdSpatialFunctionTest)
