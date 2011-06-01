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
itkHistogramToEntropyImageFilterTest1.cxx
itkHistogramToEntropyImageFilterTest2.cxx
itkHistogramToIntensityImageFilterTest1.cxx
itkHistogramToIntensityImageFilterTest2.cxx
itkHistogramToLogProbabilityImageFilterTest1.cxx
itkHistogramToLogProbabilityImageFilterTest2.cxx
itkHistogramToProbabilityImageFilterTest1.cxx
itkHistogramToProbabilityImageFilterTest2.cxx
itkAccumulateImageFilterTest.cxx
itkAdaptiveHistogramEqualizationImageFilterTest.cxx
itkNormalizedCorrelationImageFilterTest.cxx
itkGetAverageSliceImageFilterTest.cxx
itkBinaryProjectionImageFilterTest.cxx
)

CreateTestDriver(ITK-ImageStatistics  "${ITK-ImageStatistics-Test_LIBRARIES}" "${ITK-ImageStatisticsTests}")

itk_add_test(NAME itkImageStatisticsHeaderTest
      COMMAND ITK-ImageStatisticsTestDriver itkImageStatisticsHeaderTest)
itk_add_test(NAME itkStatisticsImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver itkStatisticsImageFilterTest)
itk_add_test(NAME itkLabelStatisticsImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver itkLabelStatisticsImageFilterTest
              ${ITK_DATA_ROOT}/Input/peppers.png ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png)
itk_add_test(NAME itkSumProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeSumProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeSumProjection.tif
    itkSumProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeSumProjection.tif)
itk_add_test(NAME itkStandardDeviationProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeStandardDeviationProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStandardDeviationProjection.tif
    itkStandardDeviationProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStandardDeviationProjection.tif)
itk_add_test(NAME itkMinimumMaximumImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver itkMinimumMaximumImageFilterTest)
itk_add_test(NAME itkImagePCAShapeModelEstimatorTest
      COMMAND ITK-ImageStatisticsTestDriver itkImagePCAShapeModelEstimatorTest)
itk_add_test(NAME itkMaximumProjectionImageFilterTest2_1
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection0.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection0.tif
    itkMaximumProjectionImageFilterTest2 0 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection0.tif)
itk_add_test(NAME itkMaximumProjectionImageFilterTest2_2
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection1.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection1.tif
    itkMaximumProjectionImageFilterTest2 1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection1.tif)
itk_add_test(NAME itkMaximumProjectionImageFilterTest3_1
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection2D0.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D0.tif
    itkMaximumProjectionImageFilterTest3 0 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D0.tif)
itk_add_test(NAME itkMaximumProjectionImageFilterTest3_2
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection2D1.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D1.tif
    itkMaximumProjectionImageFilterTest3 1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D1.tif)
itk_add_test(NAME itkMinimumProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMinimumProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMinimumProjection.tif
    itkMinimumProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMinimumProjection.tif)
itk_add_test(NAME itkMeanProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMeanProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMeanProjection.tif
    itkMeanProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMeanProjection.tif)
itk_add_test(NAME itkImagePCADecompositionCalculatorTest
      COMMAND ITK-ImageStatisticsTestDriver itkImagePCADecompositionCalculatorTest)
itk_add_test(NAME itkMaximumProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection.tif
    itkMaximumProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection.tif)
itk_add_test(NAME itkMedianProjectionImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMedianProjection.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMedianProjection.tif
    itkMedianProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMedianProjection.tif)
itk_add_test(NAME itkHistogramToEntropyImageFilterTest1
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToEntropyImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToEntropyImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToEntropyImageFilterTest2
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToEntropyImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToEntropyImageFilterTest2.mhd)
itk_add_test(NAME itkHistogramToIntensityImageFilterTest1
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToIntensityImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToIntensityImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToIntensityImageFilterTest2
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToIntensityImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToIntensityImageFilterTest2.mhd)
itk_add_test(NAME itkHistogramToLogProbabilityImageFilterTest1
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToLogProbabilityImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToLogProbabilityImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToLogProbabilityImageFilterTest2
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToLogProbabilityImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToLogProbabilityImageFilterTest2.mhd)
itk_add_test(NAME itkHistogramToProbabilityImageFilterTest1
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToProbabilityImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToProbabilityImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToProbabilityImageFilterTest2
      COMMAND ITK-ImageStatisticsTestDriver itkHistogramToProbabilityImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToProbabilityImageFilterTest2.mhd)
itk_add_test(NAME itkAccumulateImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png
    itkAccumulateImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png)
itk_add_test(NAME itkAdaptiveHistogramEqualizationImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png
    itkAdaptiveHistogramEqualizationImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png 10 0.5 0.5)
itk_add_test(NAME itkAdaptiveHistogramEqualizationImageFilterTest2
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png
    itkAdaptiveHistogramEqualizationImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png 10 1.0 0.25)
itk_add_test(NAME itkNormalizedCorrelationImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NormalizedCorrelationImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png
    itkNormalizedCorrelationImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png)
itk_add_test(NAME itkGetAverageSliceImageFilterTest
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png
    itkGetAverageSliceImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png)
itk_add_test(NAME itkBinaryProjectionImageFilterTest1
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection100.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection100.tif
    itkBinaryProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection100.tif 100 0)
itk_add_test(NAME itkBinaryProjectionImageFilterTest2
      COMMAND ITK-ImageStatisticsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection200.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection200.tif
    itkBinaryProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection200.tif 200 0)
