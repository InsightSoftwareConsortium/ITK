itk_module_test()
set(ITKImageStatisticsTests
itkStatisticsImageFilterTest.cxx
itkLabelStatisticsImageFilterTest.cxx
itkSumProjectionImageFilterTest.cxx
itkStandardDeviationProjectionImageFilterTest.cxx
itkImageMomentsTest.cxx
itkImageToHistogramFilterTest.cxx
itkImageToHistogramFilterTest2.cxx
itkImageToHistogramFilterTest3.cxx
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
itkGetAverageSliceImageFilterTest.cxx
itkBinaryProjectionImageFilterTest.cxx
itkProjectionImageFilterTest.cxx

itkLabelOverlapMeasuresImageFilterTest.cxx
)

CreateTestDriver(ITKImageStatistics  "${ITKImageStatistics-Test_LIBRARIES}" "${ITKImageStatisticsTests}")

itk_add_test(NAME itkStatisticsImageFilterTest
      COMMAND ITKImageStatisticsTestDriver itkStatisticsImageFilterTest)
itk_add_test(NAME itkLabelStatisticsImageFilterTest
      COMMAND ITKImageStatisticsTestDriver itkLabelStatisticsImageFilterTest
              DATA{${ITK_DATA_ROOT}/Input/peppers.png} DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png})
itk_add_test(NAME itkSumProjectionImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeSumProjection.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeSumProjection.tif
    itkSumProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeSumProjection.tif)
itk_add_test(NAME itkStandardDeviationProjectionImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeStandardDeviationProjection.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStandardDeviationProjection.tif
    itkStandardDeviationProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStandardDeviationProjection.tif)
itk_add_test(NAME itkImageMomentsTest
      COMMAND ITKImageStatisticsTestDriver itkImageMomentsTest)
itk_add_test(NAME itkImageToHistogramFilterTest
      COMMAND ITKImageStatisticsTestDriver itkImageToHistogramFilterTest)
itk_add_test(NAME itkImageToHistogramFilterTest2
      COMMAND ITKImageStatisticsTestDriver itkImageToHistogramFilterTest2
              DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest2.txt)
itk_add_test(NAME itkImageToHistogramFilterTest3
      COMMAND ITKImageStatisticsTestDriver itkImageToHistogramFilterTest3
              DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest3.txt)
itk_add_test(NAME itkMinimumMaximumImageFilterTest
      COMMAND ITKImageStatisticsTestDriver itkMinimumMaximumImageFilterTest)
itk_add_test(NAME itkImagePCAShapeModelEstimatorTest
      COMMAND ITKImageStatisticsTestDriver itkImagePCAShapeModelEstimatorTest)
itk_add_test(NAME itkMaximumProjectionImageFilterTest2_1
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection0.mha}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection0.mha
    itkMaximumProjectionImageFilterTest2 0 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection0.mha)
itk_add_test(NAME itkMaximumProjectionImageFilterTest2_2
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection1.mha}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection1.mha
    itkMaximumProjectionImageFilterTest2 1 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection1.mha)
itk_add_test(NAME itkMaximumProjectionImageFilterTest3_1
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection2D0.mha}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D0.mha
    itkMaximumProjectionImageFilterTest3 0 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D0.mha)
itk_add_test(NAME itkMaximumProjectionImageFilterTest3_2
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection2D1.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D1.tif
    itkMaximumProjectionImageFilterTest3 1 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection2D1.tif)
itk_add_test(NAME itkMinimumProjectionImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMinimumProjection.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMinimumProjection.tif
    itkMinimumProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMinimumProjection.tif)
itk_add_test(NAME itkMeanProjectionImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMeanProjection.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMeanProjection.tif
    itkMeanProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMeanProjection.tif)
itk_add_test(NAME itkImagePCADecompositionCalculatorTest
      COMMAND ITKImageStatisticsTestDriver itkImagePCADecompositionCalculatorTest)
itk_add_test(NAME itkMaximumProjectionImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMaximumProjection.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection.tif
    itkMaximumProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMaximumProjection.tif)
itk_add_test(NAME itkMedianProjectionImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeMedianProjection.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMedianProjection.tif
    itkMedianProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeMedianProjection.tif)
itk_add_test(NAME itkHistogramToEntropyImageFilterTest1
      COMMAND ITKImageStatisticsTestDriver itkHistogramToEntropyImageFilterTest1
              DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkHistogramToEntropyImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToEntropyImageFilterTest2
      COMMAND ITKImageStatisticsTestDriver itkHistogramToEntropyImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToEntropyImageFilterTest2.mhd)
itk_add_test(NAME itkHistogramToIntensityImageFilterTest1
      COMMAND ITKImageStatisticsTestDriver itkHistogramToIntensityImageFilterTest1
              DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkHistogramToIntensityImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToIntensityImageFilterTest2
      COMMAND ITKImageStatisticsTestDriver itkHistogramToIntensityImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToIntensityImageFilterTest2.mhd)
itk_add_test(NAME itkHistogramToLogProbabilityImageFilterTest1
      COMMAND ITKImageStatisticsTestDriver itkHistogramToLogProbabilityImageFilterTest1
              DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkHistogramToLogProbabilityImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToLogProbabilityImageFilterTest2
      COMMAND ITKImageStatisticsTestDriver itkHistogramToLogProbabilityImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToLogProbabilityImageFilterTest2.mhd)
itk_add_test(NAME itkHistogramToProbabilityImageFilterTest1
      COMMAND ITKImageStatisticsTestDriver itkHistogramToProbabilityImageFilterTest1
              DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkHistogramToProbabilityImageFilterTest1.mhd)
itk_add_test(NAME itkHistogramToProbabilityImageFilterTest2
      COMMAND ITKImageStatisticsTestDriver itkHistogramToProbabilityImageFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_EXAMPLE_DATA_ROOT}/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToProbabilityImageFilterTest2.mhd)

itk_add_test(NAME itkAccumulateImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png
    itkAccumulateImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm} ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png)
itk_add_test(NAME itkAdaptiveHistogramEqualizationImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png
    itkAdaptiveHistogramEqualizationImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sf4.png} ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png 10 0.5 0.5)
itk_add_test(NAME itkAdaptiveHistogramEqualizationImageFilterTest2
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png
    itkAdaptiveHistogramEqualizationImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sf4.png} ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png 10 1.0 0.25)
itk_add_test(NAME itkGetAverageSliceImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png
    itkGetAverageSliceImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm} ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png)
itk_add_test(NAME itkBinaryProjectionImageFilterTest1
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection100.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection100.tif
    itkBinaryProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection100.tif 100 0)
itk_add_test(NAME itkBinaryProjectionImageFilterTest2
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection200.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection200.tif
    itkBinaryProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection200.tif 200 0)
itk_add_test(NAME itkProjectionImageFilterTest1
      COMMAND ITKImageStatisticsTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection100.tif}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif
    itkProjectionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif 100 0)

itk_add_test( NAME itkLabelOverlapMeasuresImageFilterTest
      COMMAND ITKImageStatisticsTestDriver
      itkLabelOverlapMeasuresImageFilterTest 2
          DATA{Input/sourceImage.nii.gz}
          DATA{Input/targetImage.nii.gz} )
