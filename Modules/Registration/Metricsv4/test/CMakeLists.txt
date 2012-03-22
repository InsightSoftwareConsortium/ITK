itk_module_test()
set(ITKMetricsv4Tests
  itkEuclideanDistancePointSetMetricTest.cxx
  itkExpectationBasedPointSetMetricTest.cxx
  itkJensenHavrdaCharvatTsallisPointSetMetricTest.cxx
  itkImageToImageMetricv4Test.cxx
  itkJointHistogramMutualInformationImageToImageMetricv4Test.cxx
  itkJointHistogramMutualInformationImageToImageRegistrationTest.cxx
  itkMeanSquaresImageToImageMetricv4Test.cxx
  itkCorrelationImageToImageMetricv4Test.cxx
  itkANTSNeighborhoodCorrelationImageToImageMetricv4Test.cxx
  itkANTSNeighborhoodCorrelationImageToImageRegistrationTest.cxx
  itkMattesMutualInformationImageToImageMetricv4Test.cxx
  itkMattesMutualInformationImageToImageMetricv4RegistrationTest.cxx
  itkMultiStartImageToImageMetricv4RegistrationTest.cxx
  itkMultiGradientImageToImageMetricv4RegistrationTest.cxx
  itkMetricImageGradientTest.cxx
  itkMeanSquaresImageToImageMetricv4RegistrationTest.cxx
  itkImageToImageMetricv4RegistrationTest.cxx
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
              2 1 )

itk_add_test(NAME itkJointHistogramMutualInformationImageToImageRegistrationTest2
      COMMAND ITKMetricsv4TestDriver
              itkJointHistogramMutualInformationImageToImageRegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkJointHistogramMutualInformationImageToImageRegistrationTest2.nii.gz
              2 1 )

itk_add_test(NAME itkMeanSquaresImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
      itkMeanSquaresImageToImageMetricv4Test)

itk_add_test(NAME itkCorrelationImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
      itkCorrelationImageToImageMetricv4Test)


itk_add_test(NAME itkANTSNeighborhoodCorrelationImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
              itkANTSNeighborhoodCorrelationImageToImageMetricv4Test)

itk_add_test(NAME itkANTSNeighborhoodCorrelationImageToImageRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkANTSNeighborhoodCorrelationImageToImageRegistrationTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBSplined10.png
             ${TEMP}/itkANTSNeighborhoodCorrelationImageToImageRegistrationTest.nii.gz
              1 1 0.25 )

itk_add_test(NAME itkMattesMutualInformationImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
      itkMattesMutualInformationImageToImageMetricv4Test)

itk_add_test(NAME itkMattesMutualInformationImageToImageMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkMattesMutualInformationImageToImageMetricv4RegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkMattesMutualInformationImageToImageMetricv4RegistrationTest.nii.gz
              5 0 )

itk_add_test(NAME itkMultiStartImageToImageMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkMultiStartImageToImageMetricv4RegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkMultiStartImageToImageMetricv4RegistrationTest.nii.gz
              5 1 )

itk_add_test(NAME itkMultiGradientImageToImageMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkMultiGradientImageToImageMetricv4RegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkMultiGradientImageToImageMetricv4RegistrationTest.nii.gz
              5  )

itk_add_test(NAME itkMetricImageGradientTest
      COMMAND ITKMetricsv4TestDriver
              itkMetricImageGradientTest)

itk_add_test(NAME itkMeanSquaresImageToImageMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkMeanSquaresImageToImageMetricv4RegistrationTest
              DATA{Baseline/face_avg.jpg}
              DATA{Input/face_b.jpg}
              ${TEMP}/itkMeanSquaresImageToImageMetricv4RegistrationTest.nii.gz
              2 1 )

itk_add_test(NAME itkImageToImageMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkImageToImageMetricv4RegistrationTest
              75  #iterations1
              0.8 #maxStepSize1
              120 #iterations2
              0.1 #maxStepSize2
              0 #doSampling
              0 #useGradientFilter
              )

itk_add_test(NAME itkImageToImageMetricv4RegistrationTest2
      COMMAND ITKMetricsv4TestDriver
              itkImageToImageMetricv4RegistrationTest
              47  #iterations1
              1.0 #maxStepSize1
              120 #iterations2
              0.1 #maxStepSize2
              1 #doSampling
              0 #useGradientFilter
              )

itk_add_test(NAME itkImageToImageMetricv4RegistrationTest3
      COMMAND ITKMetricsv4TestDriver
              itkImageToImageMetricv4RegistrationTest
              50  #iterations1
              1.0 #maxStepSize1
              120 #iterations2
              0.1 #maxStepSize2
              0 #doSampling
              1 #useGradientFilter
              )
