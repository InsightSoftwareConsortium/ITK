itk_module_test()
set(ITKHighDimensionalMetricsTests
  itkEuclideanDistancePointSetMetricTest.cxx
  itkExpectationBasedPointSetMetricTest.cxx
  itkJensenHavrdaCharvatTsallisPointSetMetricTest.cxx
  itkImageToImageObjectMetricTest.cxx
  itkJointHistogramMutualInformationImageToImageObjectMetricTest.cxx
  itkJointHistogramMutualInformationImageToImageObjectRegistrationTest.cxx
  itkDemonsImageToImageObjectMetricTest.cxx
  itkANTSNeighborhoodCorrelationImageToImageObjectMetricTest.cxx
  itkANTSNeighborhoodCorrelationImageToImageObjectRegistrationTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKHighDimensionalMetrics  "${ITKHighDimensionalMetrics-Test_LIBRARIES}" "${ITKHighDimensionalMetricsTests}")

itk_add_test(NAME itkEuclideanDistancePointSetMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver itkEuclideanDistancePointSetMetricTest)

itk_add_test(NAME itkExpectationBasedPointSetMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver itkExpectationBasedPointSetMetricTest)

itk_add_test(NAME itkJensenHavrdaCharvatTsallisPointSetMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver itkJensenHavrdaCharvatTsallisPointSetMetricTest)

itk_add_test(NAME itkImageToImageObjectMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver
              itkImageToImageObjectMetricTest)

itk_add_test(NAME itkJointHistogramMutualInformationImageToImageObjectMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver
              itkJointHistogramMutualInformationImageToImageObjectMetricTest)

itk_add_test(NAME itkJointHistogramMutualInformationImageToImageObjectRegistrationTest
      COMMAND ITKHighDimensionalMetricsTestDriver
              itkJointHistogramMutualInformationImageToImageObjectRegistrationTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBSplined10.png
              ${TEMP}/itkJointHistogramMutualInformationImageToImageObjectRegistrationTest.nii.gz
              10 5 )

itk_add_test(NAME itkJointHistogramMutualInformationImageToImageObjectRegistrationTest2
      COMMAND ITKHighDimensionalMetricsTestDriver
              itkJointHistogramMutualInformationImageToImageObjectRegistrationTest
              DATA{Input/face_b.jpg}
              DATA{Baseline/face_avg.jpg}
              ${TEMP}/itkJointHistogramMutualInformationImageToImageObjectRegistrationTest2.nii.gz
              10 5 )

itk_add_test(NAME itkDemonsImageToImageObjectMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver
      itkDemonsImageToImageObjectMetricTest)

itk_add_test(NAME itkANTSNeighborhoodCorrelationImageToImageObjectMetricTest
      COMMAND ITKHighDimensionalMetricsTestDriver
              itkANTSNeighborhoodCorrelationImageToImageObjectMetricTest)

itk_add_test(NAME itkANTSNeighborhoodCorrelationImageToImageObjectRegistrationTest
      COMMAND ITKHighDimensionalMetricsTestDriver
              itkANTSNeighborhoodCorrelationImageToImageObjectRegistrationTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBSplined10.png
             ${TEMP}/itkANTSNeighborhoodCorrelationImageToImageObjectRegistrationTest.nii.gz
              10 1 0.25 )
