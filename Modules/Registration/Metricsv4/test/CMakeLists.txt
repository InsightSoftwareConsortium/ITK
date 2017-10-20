itk_module_test()
set(ITKMetricsv4Tests
  itkEuclideanDistancePointSetMetricTest.cxx
  itkExpectationBasedPointSetMetricTest.cxx
  itkJensenHavrdaCharvatTsallisPointSetMetricTest.cxx
  itkJensenHavrdaCharvatTsallisPointSetMetricRegistrationTest.cxx
  itkLabeledPointSetMetricTest.cxx
  itkLabeledPointSetMetricRegistrationTest.cxx
  itkImageToImageMetricv4Test.cxx
  itkJointHistogramMutualInformationImageToImageMetricv4Test.cxx
  itkJointHistogramMutualInformationImageToImageRegistrationTest.cxx
  itkMeanSquaresImageToImageMetricv4Test.cxx
  itkCorrelationImageToImageMetricv4Test.cxx
  itkMeanSquaresImageToImageMetricv4OnVectorTest.cxx
  itkMeanSquaresImageToImageMetricv4OnVectorTest2.cxx
  itkANTSNeighborhoodCorrelationImageToImageMetricv4Test.cxx
  itkANTSNeighborhoodCorrelationImageToImageRegistrationTest.cxx
  itkMattesMutualInformationImageToImageMetricv4Test.cxx
  itkMattesMutualInformationImageToImageMetricv4RegistrationTest.cxx
  itkMultiStartImageToImageMetricv4RegistrationTest.cxx
  itkMultiGradientImageToImageMetricv4RegistrationTest.cxx
  itkMetricImageGradientTest.cxx
  itkMeanSquaresImageToImageMetricv4RegistrationTest.cxx
  itkMeanSquaresImageToImageMetricv4RegistrationTest2.cxx
  itkImageToImageMetricv4RegistrationTest.cxx
  itkDemonsImageToImageMetricv4Test.cxx
  itkDemonsImageToImageMetricv4RegistrationTest.cxx
  itkEuclideanDistancePointSetMetricRegistrationTest.cxx
  itkExpectationBasedPointSetMetricRegistrationTest.cxx
  itkEuclideanDistancePointSetMetricTest2.cxx
  itkObjectToObjectMultiMetricv4Test.cxx
  itkObjectToObjectMultiMetricv4RegistrationTest.cxx
  itkMeanSquaresImageToImageMetricv4SpeedTest.cxx
  itkMeanSquaresImageToImageMetricv4VectorRegistrationTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKMetricsv4  "${ITKMetricsv4-Test_LIBRARIES}" "${ITKMetricsv4Tests}")

itk_add_test(NAME itkEuclideanDistancePointSetMetricTest
      COMMAND ITKMetricsv4TestDriver itkEuclideanDistancePointSetMetricTest)

itk_add_test(NAME itkEuclideanDistancePointSetMetricTest2
      COMMAND ITKMetricsv4TestDriver itkEuclideanDistancePointSetMetricTest2)

itk_add_test(NAME itkExpectationBasedPointSetMetricTest
      COMMAND ITKMetricsv4TestDriver itkExpectationBasedPointSetMetricTest)

itk_add_test(NAME itkJensenHavrdaCharvatTsallisPointSetMetricTest
      COMMAND ITKMetricsv4TestDriver itkJensenHavrdaCharvatTsallisPointSetMetricTest)

itk_add_test(NAME itkJensenHavrdaCharvatTsallisPointSetMetricRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkJensenHavrdaCharvatTsallisPointSetMetricRegistrationTest)

itk_add_test(NAME itkLabeledPointSetMetricTest
      COMMAND ITKMetricsv4TestDriver itkLabeledPointSetMetricTest)

itk_add_test(NAME itkLabeledPointSetMetricRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkLabeledPointSetMetricRegistrationTest)

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

itk_add_test(NAME itkMeanSquaresImageToImageMetricv4OnVectorTest
      COMMAND ITKMetricsv4TestDriver
      itkMeanSquaresImageToImageMetricv4OnVectorTest)

itk_add_test(NAME itkMeanSquaresImageToImageMetricv4OnVectorTest2
      COMMAND ITKMetricsv4TestDriver
      itkMeanSquaresImageToImageMetricv4OnVectorTest2)

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

itk_add_test(NAME itkMeanSquaresImageToImageMetricv4RegistrationTest2
  COMMAND ITKMetricsv4TestDriver
  itkMeanSquaresImageToImageMetricv4RegistrationTest2
              DATA{${INPUTDATA}/r16slice.nii.gz}
              DATA{${INPUTDATA}/r64slice.nii.gz}
              ${TEMP}/itkMeanSquaresImageToImageMetricv4RegistrationTest2.nii.gz
              )

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

itk_add_test(NAME itkDemonsImageToImageMetricv4Test
      COMMAND ITKMetricsv4TestDriver
              itkDemonsImageToImageMetricv4Test)

itk_add_test(NAME itkDemonsImageToImageMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
      --with-threads 1
      --compare DATA{Baseline/itkDemonsImageToImageMetricv4RegistrationTest_I35_S0_T1_F0.nii.gz}
                ${TEMP}/itkDemonsImageToImageMetricv4RegistrationTest.nii.gz
              itkDemonsImageToImageMetricv4RegistrationTest
              DATA{${INPUTDATA}/r16slice.nii.gz}
              DATA{${INPUTDATA}/r64slice.nii.gz}
              ${TEMP}/itkDemonsImageToImageMetricv4RegistrationTest.nii.gz
              35 #iterations
              0  #sampling
              0  #image gradient filter
              )

itk_add_test(NAME itkDemonsImageToImageMetricv4RegistrationTest2
      COMMAND ITKMetricsv4TestDriver
      --with-threads 1
      --compare DATA{Baseline/itkDemonsImageToImageMetricv4RegistrationTest_I35_S1_T1_F0.nii.gz}
                ${TEMP}/itkDemonsImageToImageMetricv4RegistrationTest2.nii.gz
              itkDemonsImageToImageMetricv4RegistrationTest
              DATA{${INPUTDATA}/r16slice.nii.gz}
              DATA{${INPUTDATA}/r64slice.nii.gz}
              ${TEMP}/itkDemonsImageToImageMetricv4RegistrationTest2.nii.gz
              35 #iterations
              1  #sampling
              0  #image gradient filter
              )

itk_add_test(NAME itkDemonsImageToImageMetricv4RegistrationTest3
      COMMAND ITKMetricsv4TestDriver
      --with-threads 1
      --compare DATA{Baseline/itkDemonsImageToImageMetricv4RegistrationTest_I35_S0_T1_F1.nii.gz}
                ${TEMP}/itkDemonsImageToImageMetricv4RegistrationTest3.nii.gz
              itkDemonsImageToImageMetricv4RegistrationTest
              DATA{${INPUTDATA}/r16slice.nii.gz}
              DATA{${INPUTDATA}/r64slice.nii.gz}
              ${TEMP}/itkDemonsImageToImageMetricv4RegistrationTest3.nii.gz
              35 #iterations
              0  #sampling
              1  #image gradient filter
              )

itk_add_test(NAME itkEuclideanDistancePointSetMetricRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkEuclideanDistancePointSetMetricRegistrationTest)

itk_add_test(NAME itkExpectationBasedPointSetMetricRegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkExpectationBasedPointSetMetricRegistrationTest)

itk_add_test(NAME itkObjectToObjectMultiMetricv4Test
      COMMAND ITKMetricsv4TestDriver
              itkObjectToObjectMultiMetricv4Test)

itk_add_test(NAME itkObjectToObjectMultiMetricv4RegistrationTest
      COMMAND ITKMetricsv4TestDriver
              itkObjectToObjectMultiMetricv4RegistrationTest )

itk_add_test(NAME itkMeanSquaresImageToImageMetricv4VectorRegistrationTest
      COMMAND ITKMetricsv4TestDriver
      --with-threads 1
      --compare DATA{Baseline/itkMeanSquaresImageToImageMetricv4VectorRegistrationTest.nii.gz}
                ${TEMP}/itkMeanSquaresImageToImageMetricv4VectorRegistrationTest.nii.gz
              itkMeanSquaresImageToImageMetricv4VectorRegistrationTest
              DATA{Input/orange.jpg}
              DATA{Input/apple.jpg}
              ${TEMP}/itkMeanSquaresImageToImageMetricv4VectorRegistrationTest.nii.gz
              100 25 )

itk_python_expression_add_test(NAME itkDemonsImageToImageMetricv4PythonTest
  EXPRESSION "instance = itk.DemonsImageToImageMetricv4.New()")
itk_python_expression_add_test(NAME itkCorrelationImageToImageMetricv4PythonTest
  EXPRESSION "instance = itk.CorrelationImageToImageMetricv4.New()")
itk_python_expression_add_test(NAME itkJointHistogramMutualInformationImageToImageMetricv4PythonTest
  EXPRESSION "instance = itk.JointHistogramMutualInformationImageToImageMetricv4.New()")
