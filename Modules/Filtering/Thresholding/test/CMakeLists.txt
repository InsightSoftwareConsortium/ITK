itk_module_test()
set(ITKThresholdingTests
itkOtsuThresholdImageCalculatorTest.cxx
itkOtsuMultipleThresholdsImageFilterTest.cxx
itkOtsuThresholdImageFilterTest.cxx
itkThresholdLabelerImageFilterTest.cxx
itkBinaryThresholdImageFilterTest.cxx
itkThresholdImageFilterTest.cxx
itkOtsuMultipleThresholdsCalculatorTest.cxx
itkBinaryThresholdImageFilterTest2.cxx
itkBinaryThresholdProjectionImageFilterTest.cxx
itkBinaryThresholdSpatialFunctionTest.cxx
)

CreateTestDriver(ITKThresholding  "${ITKThresholding-Test_LIBRARIES}" "${ITKThresholdingTests}")

itk_add_test(NAME itkOtsuThresholdImageCalculatorTest
      COMMAND ITKThresholdingTestDriver itkOtsuThresholdImageCalculatorTest)
itk_add_test(NAME itkOtsuMultipleThresholdsImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
    itkOtsuMultipleThresholdsImageFilterTest ${ITK_DATA_ROOT}/Input/peppers.png ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png 64 3 2)
itk_add_test(NAME itkOtsuThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuThresholdImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
    itkOtsuThresholdImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png 64)
itk_add_test(NAME itkThresholdLabelerImageFilterTest
      COMMAND ITKThresholdingTestDriver itkThresholdLabelerImageFilterTest)
itk_add_test(NAME itkBinaryThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver itkBinaryThresholdImageFilterTest)
itk_add_test(NAME itkThresholdImageFilterTest
      COMMAND ITKThresholdingTestDriver itkThresholdImageFilterTest)
itk_add_test(NAME itkOtsuMultipleThresholdsCalculatorTest
      COMMAND ITKThresholdingTestDriver itkOtsuMultipleThresholdsCalculatorTest)
itk_add_test(NAME itkBinaryThresholdImageFilterTest2
      COMMAND ITKThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png
    itkBinaryThresholdImageFilterTest2 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png)
itk_add_test(NAME itkBinaryThresholdProjectionImageFilterTest
      COMMAND ITKThresholdingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryThresholdProjection.png
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png
    itkBinaryThresholdProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png 100 255 0)
itk_add_test(NAME itkBinaryThresholdSpatialFunctionTest
      COMMAND ITKThresholdingTestDriver itkBinaryThresholdSpatialFunctionTest)
