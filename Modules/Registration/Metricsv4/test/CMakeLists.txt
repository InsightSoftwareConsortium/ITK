itk_module_test()
set(ITKMetricsv4Tests
  itkEuclideanDistancePointSetMetricTest.cxx
  itkExpectationBasedPointSetMetricTest.cxx
  itkJensenHavrdaCharvatTsallisPointSetMetricTest.cxx
  itkImageToImageMetricv4Test.cxx
  itkJointHistogramMutualInformationImageToImageMetricv4Test.cxx
  itkJointHistogramMutualInformationImageToImageRegistrationTest.cxx
  itkDemonsImageToImageMetricv4Test.cxx
  itkANTSNeighborhoodCorrelationImageToImageMetricv4Test.cxx
  itkANTSNeighborhoodCorrelationImageToImageRegistrationTest.cxx
  itkAffineDemonsImageToImageRegistrationTest.cxx
  itkMattesMutualInformationImageToImageMetricv4Test.cxx
  itkMattesMutualInformationImageToImageMetricv4RegistrationTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKMetricsv4  "${ITKMetricsv4-Test_LIBRARIES}" "${ITKMetricsv4Tests}")

itk_add_test(NAME itkEuclideanDistancePointSetMetricTest
      COMMAND ITKMetricsv4TestDriver itkEuclideanDistancePointSetMetricTest)

itk_add_test(NAME itkExpectationBasedPointSetMetricTest
      COMMAND ITKMetricsv4TestDriver itkExpectationBasedPointSetMetricTest)

itk_add_test(NAME itkJensenHavrdaCharvatTsallisPointSetMetricTest
      COMMAND ITKMetricsv4TestDriver itkJensenHavrdaCharvatTsallisPointSetMetricTest)

itk_add_test(NAME itkImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
              itkImageToImageMetricv4Test)

itk_add_test(NAME itkJointHistogramMutualInformationImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
              itkJointHistogramMutualInformationImageToImageMetricv4Test)

itk_add_test(NAME itkJointHistogramMutualInformationImageToImageRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkJointHistogramMutualInformationImageToImageRegistrationTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBSplined10.png
              ${TEMP}/itkJointHistogramMutualInformationImageToImageRegistrationTest.nii.gz
              10 5 )

itk_add_test(NAME itkJointHistogramMutualInformationImageToImageRegistrationTest2
      COMMAND ITKMetricsv4TestDriver
              itkJointHistogramMutualInformationImageToImageRegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkJointHistogramMutualInformationImageToImageRegistrationTest2.nii.gz
              50 0 )

itk_add_test(NAME itkDemonsImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
      itkDemonsImageToImageMetricv4Test)

itk_add_test(NAME itkANTSNeighborhoodCorrelationImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
              itkANTSNeighborhoodCorrelationImageToImageMetricv4Test)

itk_add_test(NAME itkANTSNeighborhoodCorrelationImageToImageRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkANTSNeighborhoodCorrelationImageToImageRegistrationTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBSplined10.png
             ${TEMP}/itkANTSNeighborhoodCorrelationImageToImageRegistrationTest.nii.gz
              10 1 0.25 )

itk_add_test(NAME itkAffineDemonsImageToImageRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkAffineDemonsImageToImageRegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkAffineDemonsImageToImageRegistrationTest.nii.gz
              30 )

itk_add_test(NAME itkMattesMutualInformationImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
      itkMattesMutualInformationImageToImageMetricv4Test 0)

itk_add_test(NAME itkMattesMutualInformationImageToImageMetricv4Test2
      COMMAND ITKMetricsv4TestDriver
      itkMattesMutualInformationImageToImageMetricv4Test 1)

itk_add_test(NAME itkMattesMutualInformationImageToImageMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkMattesMutualInformationImageToImageMetricv4RegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkMattesMutualInformationImageToImageMetricv4RegistrationTest.nii.gz
              5 0 )
