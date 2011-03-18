itk_module_test()
set(ITK-ImageStatisticsTests
itkStatisticsImageFilterTest.cxx
itkLabelStatisticsImageFilterTest.cxx
itkSumProjectionImageFilterTest.cxx
itkStandardDeviationProjectionImageFilterTest.cxx
itkImageStatisticsHeaderTest.cxx
itkMinimumMaximumImageFilterTest.cxx
itkImagePCAShapeModelEstimatorTest.cxx
itkMaximumProjectionImageFilterTest2.cxx
itkMaximumProjectionImageFilterTest3.cxx
itkMinimumProjectionImageFilterTest.cxx
itkMeanProjectionImageFilterTest.cxx
itkImagePCADecompositionCalculatorTest.cxx
itkMaximumProjectionImageFilterTest.cxx
itkMedianProjectionImageFilterTest.cxx
)

CreateTestDriver(ITK-ImageStatistics  "${ITK-ImageStatistics-Test_LIBRARIES}" "${ITK-ImageStatisticsTests}")

add_test(NAME itkImageStatisticsHeaderTest
      COMMAND ITK-ImageStatisticsTestDriver itkImageStatisticsHeaderTest)
add_test(NAME itkStatisticsImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver itkStatisticsImageFilterTest)
add_test(NAME itkLabelStatisticsImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver itkLabelStatisticsImageFilterTest
              ${ITK_DATA_ROOT}/Input/peppers.png ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png)
add_test(NAME itkSumProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeSumProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeSumProjection.tif
    itkSumProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeSumProjection.tif)
add_test(NAME itkStandardDeviationProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeStandardDeviationProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStandardDeviationProjection.tif
    itkStandardDeviationProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStandardDeviationProjection.tif)
add_test(NAME itkMinimumMaximumImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver itkMinimumMaximumImageFilterTest)
add_test(NAME itkImagePCAShapeModelEstimatorTest
      COMMAND ITK-ImageStatisticsTestDriver itkImagePCAShapeModelEstimatorTest)
add_test(NAME itkMaximumProjectionImageFilterTest2_1
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection0.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection0.tif
    itkMaximumProjectionImageFilterTest2 0 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection0.tif)
add_test(NAME itkMaximumProjectionImageFilterTest2_2
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection1.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection1.tif
    itkMaximumProjectionImageFilterTest2 1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection1.tif)
add_test(NAME itkMaximumProjectionImageFilterTest3_1
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection2D0.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D0.tif
    itkMaximumProjectionImageFilterTest3 0 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D0.tif)
add_test(NAME itkMaximumProjectionImageFilterTest3_2
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection2D1.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D1.tif
    itkMaximumProjectionImageFilterTest3 1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D1.tif)
add_test(NAME itkMinimumProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMinimumProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMinimumProjection.tif
    itkMinimumProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMinimumProjection.tif)
add_test(NAME itkMeanProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMeanProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMeanProjection.tif
    itkMeanProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMeanProjection.tif)
add_test(NAME itkImagePCADecompositionCalculatorTest
      COMMAND ITK-ImageStatisticsTestDriver itkImagePCADecompositionCalculatorTest)
add_test(NAME itkMaximumProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection.tif
    itkMaximumProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection.tif)
add_test(NAME itkMedianProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMedianProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMedianProjection.tif
    itkMedianProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMedianProjection.tif)
