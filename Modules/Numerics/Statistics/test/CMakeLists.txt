itk_module_test()
set(ITKStatisticsTests
itkDecisionRuleTest.cxx
itkDenseFrequencyContainer2Test.cxx
itkExpectationMaximizationMixtureModelEstimatorTest.cxx
itkGaussianDistributionTest.cxx
itkGaussianMembershipFunctionTest.cxx
itkGaussianMixtureModelComponentTest.cxx
itkGaussianRandomSpatialNeighborSubsamplerTest.cxx
itkKalmanLinearEstimatorTest.cxx
itkKdTreeBasedKmeansEstimatorTest.cxx
itkKdTreeGeneratorTest.cxx
itkKdTreeTest1.cxx
itkKdTreeTest2.cxx
itkKdTreeTest3.cxx
itkKdTreeTestSamplePoints.cxx
itkMaximumDecisionRuleTest.cxx
itkMinimumDecisionRuleTest.cxx
itkMaximumRatioDecisionRuleTest.cxx
itkMeanSampleFilterTest.cxx
itkMeanSampleFilterTest2.cxx
itkMeanSampleFilterTest3.cxx
itkHistogramTest.cxx
itkHistogramToTextureFeaturesFilterTest.cxx
itkHistogramToTextureFeaturesFilterNaNTest.cxx
itkChiSquareDistributionTest.cxx
itkCovarianceSampleFilterTest.cxx
itkCovarianceSampleFilterTest2.cxx
itkCovarianceSampleFilterTest3.cxx
itkWeightedCovarianceSampleFilterTest.cxx
itkWeightedCovarianceSampleFilterTest2.cxx
itkWeightedMeanSampleFilterTest.cxx
itkWeightedCentroidKdTreeGeneratorTest1.cxx
itkWeightedCentroidKdTreeGeneratorTest8.cxx
itkWeightedCentroidKdTreeGeneratorTest9.cxx
itkMahalanobisDistanceMetricTest.cxx
itkManhattanDistanceMetricTest.cxx
itkMembershipFunctionBaseTest.cxx
itkMembershipFunctionBaseTest2.cxx
itkMembershipSampleTest1.cxx
itkMembershipSampleTest2.cxx
itkMembershipSampleTest3.cxx
itkMembershipSampleTest4.cxx
itkMeasurementVectorTraitsTest.cxx
itkNeighborhoodSamplerTest1.cxx
itkMixtureModelComponentBaseTest.cxx
itkNormalVariateGeneratorTest1.cxx
itkDistanceMetricTest.cxx
itkDistanceMetricTest2.cxx
itkDistanceToCentroidMembershipFunctionTest.cxx
itkEuclideanDistanceMetricTest.cxx
itkEuclideanSquareDistanceMetricTest.cxx
itkListSampleTest.cxx
itkImageToListSampleAdaptorTest.cxx
itkImageToListSampleAdaptorTest2.cxx
itkImageToListSampleFilterTest.cxx
itkImageToListSampleFilterTest2.cxx
itkImageToListSampleFilterTest3.cxx
itkListSampleTest.cxx
itkJointDomainImageToListSampleAdaptorTest.cxx
itkPointSetToListSampleAdaptorTest.cxx
itkProbabilityDistributionTest.cxx
itkRandomVariateGeneratorBaseTest.cxx
itkSampleTest.cxx
itkSampleTest2.cxx
itkSampleTest3.cxx
itkSampleTest4.cxx
itkSampleToHistogramFilterTest.cxx
itkSampleToHistogramFilterTest2.cxx
itkSampleToHistogramFilterTest3.cxx
itkSampleToHistogramFilterTest4.cxx
itkSampleToHistogramFilterTest5.cxx
itkSampleToHistogramFilterTest6.cxx
itkSampleToHistogramFilterTest7.cxx
itkSampleToSubsampleFilterTest1.cxx
itkScalarImageToCooccurrenceListSampleFilterTest.cxx
itkScalarImageToCooccurrenceMatrixFilterTest.cxx
itkScalarImageToCooccurrenceMatrixFilterTest2.cxx
itkScalarImageToTextureFeaturesFilterTest.cxx
itkScalarImageToRunLengthMatrixFilterTest.cxx
itkScalarImageToRunLengthFeaturesFilterTest.cxx
itkSparseFrequencyContainer2Test.cxx
itkSpatialNeighborSubsamplerTest.cxx
itkStandardDeviationPerComponentSampleFilterTest.cxx
itkStatisticsTypesTest.cxx
itkSubsampleTest.cxx
itkSubsampleTest2.cxx
itkSubsampleTest3.cxx
itkTDistributionTest.cxx
itkStatisticsAlgorithmTest.cxx
itkStatisticsAlgorithmTest2.cxx
itkUniformRandomSpatialNeighborSubsamplerTest.cxx
itkVectorContainerToListSampleAdaptorTest.cxx
)

CreateTestDriver(ITKStatistics  "${ITKStatistics-Test_LIBRARIES}" "${ITKStatisticsTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkDecisionRuleTest
     COMMAND ITKStatisticsTestDriver itkDecisionRuleTest)
itk_add_test(NAME itkDenseFrequencyContainer2Test
      COMMAND ITKStatisticsTestDriver itkDenseFrequencyContainer2Test)
itk_add_test(NAME itkExpectationMaximizationMixtureModelEstimatorTest
      COMMAND ITKStatisticsTestDriver itkExpectationMaximizationMixtureModelEstimatorTest
              DATA{${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat})
itk_add_test(NAME itkGaussianDistributionTest
      COMMAND ITKStatisticsTestDriver itkGaussianDistributionTest)
itk_add_test(NAME itkGaussianMembershipFunctionTest
      COMMAND ITKStatisticsTestDriver itkGaussianMembershipFunctionTest)
itk_add_test(NAME itkGaussianMixtureModelComponentTest
      COMMAND ITKStatisticsTestDriver itkGaussianMixtureModelComponentTest
              DATA{${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat})
itk_add_test(NAME itkGaussianRandomSpatialNeighborSubsamplerTest
      COMMAND ITKStatisticsTestDriver
    --compare DATA{Baseline/itkGaussianRandomSubsamplingTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkGaussianRandomSubsamplingTest.mha
    itkGaussianRandomSpatialNeighborSubsamplerTest
     ${ITK_TEST_OUTPUT_DIR}/itkGaussianRandomSubsamplingTest.mha)
itk_add_test(NAME itkKalmanLinearEstimatorTest
      COMMAND ITKStatisticsTestDriver itkKalmanLinearEstimatorTest)
itk_add_test(NAME itkKdTreeBasedKmeansEstimatorTest
      COMMAND ITKStatisticsTestDriver itkKdTreeBasedKmeansEstimatorTest
              DATA{${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat} 1 28.54746 0.07)
itk_add_test(NAME itkKdTreeGeneratorTest
      COMMAND ITKStatisticsTestDriver itkKdTreeGeneratorTest
              DATA{${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat})

itk_add_test(NAME itkKdTreeTest1
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest1.txt
      itkKdTreeTest1
              100 100 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.dot)
set_tests_properties(itkKdTreeTest1 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest1.txt)

itk_add_test(NAME itkKdTreeTest2
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest2.txt
      itkKdTreeTest2
              DATA{${ITK_DATA_ROOT}/Input/pointsForKdTree.txt} 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.dot)
set_tests_properties(itkKdTreeTest2 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest2.txt)

itk_add_test(NAME itkKdTreeTest3
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest3.txt
      itkKdTreeTest2
              DATA{${ITK_DATA_ROOT}/Input/pointsForKdTree2.txt} 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.dot)
set_tests_properties(itkKdTreeTest3 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest3.txt)

itk_add_test(NAME itkKdTreeTest4
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest4.txt
      itkKdTreeTest1
              1000 1000 1)
set_tests_properties(itkKdTreeTest4 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest4.txt)

itk_add_test(NAME itkKdTreeTest5
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest5.txt
      itkKdTreeTest1
              1000 1000 2)
set_tests_properties(itkKdTreeTest5 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest5.txt)

itk_add_test(NAME itkKdTreeTest6
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest6.txt
      itkKdTreeTest1
              1000 1000 3)
set_tests_properties(itkKdTreeTest6 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest6.txt)

itk_add_test(NAME itkKdTreeTest7
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest7.txt
      itkKdTreeTest1
              1000 1000 5)
set_tests_properties(itkKdTreeTest7 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest7.txt)

itk_add_test(NAME itkKdTreeTest8
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest8.txt
      itkKdTreeTest1
              1000 1000 10)
set_tests_properties(itkKdTreeTest8 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest8.txt)

itk_add_test(NAME itkKdTreeTest9
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest9.txt
      itkKdTreeTest1
              1000 1000 20)
set_tests_properties(itkKdTreeTest9 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest9.txt)

itk_add_test(NAME itkKdTreeTest10
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest10.txt
      itkKdTreeTest1
              1000 1000 100)
set_tests_properties(itkKdTreeTest10 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest10.txt)

itk_add_test(NAME itkKdTreeTest11
      COMMAND ITKStatisticsTestDriver --redirectOutput ${TEMP}/itkKdTreeTest11.txt
      itkKdTreeTest3
              1000 100 20 16)
set_tests_properties(itkKdTreeTest11 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkKdTreeTest11.txt)


#
#  The following tests generate graphical plots of the KdTree organization.
#  They are very helpful for debugging and verification purposes, but not so
#  much for regression testing. They need the "dot" tool that is part of the
#  Graphviz package. The dot executable is found as part of the process of
#  configuring the Doxygen documentation. Therefore, to enable these tests
#  below, you must enable BUILD_DOXYGEN and BUILD_DOCUMENTATION.
#
if(DOXYGEN_DOT_EXECUTABLE)
itk_add_test(NAME itkKdTreeTest1Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.dot)
set_tests_properties( itkKdTreeTest1Plot PROPERTIES DEPENDS itkKdTreeTest1 )
itk_add_test(NAME itkKdTreeTest2Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.dot)
set_tests_properties( itkKdTreeTest2Plot PROPERTIES DEPENDS itkKdTreeTest2 )
itk_add_test(NAME itkKdTreeTest3Plot
      COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.png ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.dot)
set_tests_properties( itkKdTreeTest3Plot PROPERTIES DEPENDS itkKdTreeTest3 )
endif()

itk_add_test(NAME itkKdTreeTestSamplePoints
      COMMAND ITKStatisticsTestDriver itkKdTreeTestSamplePoints)
itk_add_test(NAME itkMaximumDecisionRuleTest
      COMMAND ITKStatisticsTestDriver itkMaximumDecisionRuleTest)
itk_add_test(NAME itkMinimumDecisionRuleTest
      COMMAND ITKStatisticsTestDriver itkMinimumDecisionRuleTest)
itk_add_test(NAME itkMaximumRatioDecisionRuleTest
      COMMAND ITKStatisticsTestDriver itkMaximumRatioDecisionRuleTest)
itk_add_test(NAME itkMeanSampleFilterTest
      COMMAND ITKStatisticsTestDriver itkMeanSampleFilterTest)
itk_add_test(NAME itkMeanSampleFilterTest2
      COMMAND ITKStatisticsTestDriver itkMeanSampleFilterTest2)
itk_add_test(NAME itkMeanSampleFilterTest3
      COMMAND ITKStatisticsTestDriver itkMeanSampleFilterTest3)
itk_add_test(NAME itkHistogramTest
      COMMAND ITKStatisticsTestDriver itkHistogramTest)
itk_add_test(NAME itkHistogramToTextureFeaturesFilterTest
      COMMAND ITKStatisticsTestDriver itkHistogramToTextureFeaturesFilterTest)
itk_add_test(NAME itkHistogramToTextureFeaturesFilterNaNTest
      COMMAND ITKStatisticsTestDriver itkHistogramToTextureFeaturesFilterNaNTest)
itk_add_test(NAME itkChiSquareDistributionTest
      COMMAND ITKStatisticsTestDriver itkChiSquareDistributionTest)
itk_add_test(NAME itkCovarianceSampleFilterTest
      COMMAND ITKStatisticsTestDriver itkCovarianceSampleFilterTest)
itk_add_test(NAME itkCovarianceSampleFilterTest2
      COMMAND ITKStatisticsTestDriver itkCovarianceSampleFilterTest2)
itk_add_test(NAME itkCovarianceSampleFilterTest3
      COMMAND ITKStatisticsTestDriver itkCovarianceSampleFilterTest3)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest1
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 1)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest2
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 2)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest3
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 3)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest4
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 5)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest5
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 10)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest6
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 20)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest7
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 100)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest8
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest8
              1000 1000 1)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest9
      COMMAND ITKStatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest9
              1000 1000 1)
itk_add_test(NAME itkWeightedCovarianceSampleFilterTest
      COMMAND ITKStatisticsTestDriver itkWeightedCovarianceSampleFilterTest)
itk_add_test(NAME itkWeightedCovarianceSampleFilterTest2
      COMMAND ITKStatisticsTestDriver itkWeightedCovarianceSampleFilterTest2)
itk_add_test(NAME itkWeightedMeanSampleFilterTest
      COMMAND ITKStatisticsTestDriver itkWeightedMeanSampleFilterTest)
itk_add_test(NAME itkDistanceMetricTest
      COMMAND ITKStatisticsTestDriver itkDistanceMetricTest)
itk_add_test(NAME itkDistanceMetricTest2
      COMMAND ITKStatisticsTestDriver itkDistanceMetricTest2)
itk_add_test(NAME itkDistanceToCentroidMembershipFunctionTest
      COMMAND ITKStatisticsTestDriver itkDistanceToCentroidMembershipFunctionTest)
itk_add_test(NAME itkEuclideanDistanceMetricTest
      COMMAND ITKStatisticsTestDriver itkEuclideanDistanceMetricTest)
itk_add_test(NAME itkEuclideanSquareDistanceMetricTest
      COMMAND ITKStatisticsTestDriver itkEuclideanSquareDistanceMetricTest)
itk_add_test(NAME itkListSampleTest1
      COMMAND ITKStatisticsTestDriver itkListSampleTest
              1)
itk_add_test(NAME itkListSampleTest2
      COMMAND ITKStatisticsTestDriver itkListSampleTest
              5)
itk_add_test(NAME itkListSampleTest3
      COMMAND ITKStatisticsTestDriver itkListSampleTest
              10)
itk_add_test(NAME itkListSampleTest4
      COMMAND ITKStatisticsTestDriver itkListSampleTest
              100)
itk_add_test(NAME itkImageToListSampleAdaptorTest
      COMMAND ITKStatisticsTestDriver itkImageToListSampleAdaptorTest)
itk_add_test(NAME itkImageToListSampleAdaptorTest2
      COMMAND ITKStatisticsTestDriver itkImageToListSampleAdaptorTest2)
itk_add_test(NAME itkImageToListSampleFilterTest
      COMMAND ITKStatisticsTestDriver itkImageToListSampleFilterTest)
itk_add_test(NAME itkImageToListSampleFilterTest2
      COMMAND ITKStatisticsTestDriver itkImageToListSampleFilterTest2)
itk_add_test(NAME itkImageToListSampleFilterTest3
      COMMAND ITKStatisticsTestDriver itkImageToListSampleFilterTest3)
itk_add_test(NAME itkJointDomainImageToListSampleAdaptorTest
      COMMAND ITKStatisticsTestDriver itkJointDomainImageToListSampleAdaptorTest)
itk_add_test(NAME itkPointSetToListSampleAdaptorTest
      COMMAND ITKStatisticsTestDriver itkPointSetToListSampleAdaptorTest)
itk_add_test(NAME itkProbabilityDistributionTest
      COMMAND ITKStatisticsTestDriver itkProbabilityDistributionTest)
itk_add_test(NAME itkRandomVariateGeneratorBaseTest
      COMMAND ITKStatisticsTestDriver itkRandomVariateGeneratorBaseTest)
itk_add_test(NAME itkMahalanobisDistanceMetricTest
      COMMAND ITKStatisticsTestDriver itkMahalanobisDistanceMetricTest)
itk_add_test(NAME itkManhattanDistanceMetricTest
      COMMAND ITKStatisticsTestDriver itkManhattanDistanceMetricTest)
itk_add_test(NAME itkMembershipFunctionBaseTest
      COMMAND ITKStatisticsTestDriver itkMembershipFunctionBaseTest)
itk_add_test(NAME itkMembershipFunctionBaseTest2
      COMMAND ITKStatisticsTestDriver itkMembershipFunctionBaseTest2)
itk_add_test(NAME itkMembershipSampleTest1
      COMMAND ITKStatisticsTestDriver itkMembershipSampleTest1)
itk_add_test(NAME itkMembershipSampleTest2
      COMMAND ITKStatisticsTestDriver itkMembershipSampleTest2)
itk_add_test(NAME itkMembershipSampleTest3
      COMMAND ITKStatisticsTestDriver itkMembershipSampleTest3)
itk_add_test(NAME itkMembershipSampleTest4
      COMMAND ITKStatisticsTestDriver itkMembershipSampleTest4)
itk_add_test(NAME itkMeasurementVectorTraitsTest
      COMMAND ITKStatisticsTestDriver itkMeasurementVectorTraitsTest)
itk_add_test(NAME itkNeighborhoodSamplerTest1
      COMMAND ITKStatisticsTestDriver itkNeighborhoodSamplerTest1)
itk_add_test(NAME itkMixtureModelComponentBaseTest
      COMMAND ITKStatisticsTestDriver itkMixtureModelComponentBaseTest)
itk_add_test(NAME itkNormalVariateGeneratorTest1
      COMMAND ITKStatisticsTestDriver itkNormalVariateGeneratorTest1)
itk_add_test(NAME itkSampleTest
      COMMAND ITKStatisticsTestDriver itkSampleTest)
itk_add_test(NAME itkSampleTest2
      COMMAND ITKStatisticsTestDriver itkSampleTest2)
itk_add_test(NAME itkSampleTest3
      COMMAND ITKStatisticsTestDriver itkSampleTest3)
itk_add_test(NAME itkSampleTest4
      COMMAND ITKStatisticsTestDriver itkSampleTest4)
itk_add_test(NAME itkSampleToHistogramFilterTest
      COMMAND ITKStatisticsTestDriver itkSampleToHistogramFilterTest)
itk_add_test(NAME itkSampleToHistogramFilterTest2
      COMMAND ITKStatisticsTestDriver itkSampleToHistogramFilterTest2)
itk_add_test(NAME itkSampleToHistogramFilterTest3
      COMMAND ITKStatisticsTestDriver itkSampleToHistogramFilterTest3)
# There is a bug in VS2008, Release build that causes this test to fail.
if(NOT MSVC90)
itk_add_test(NAME itkSampleToHistogramFilterTest4
      COMMAND ITKStatisticsTestDriver itkSampleToHistogramFilterTest4)
endif()
itk_add_test(NAME itkSampleToHistogramFilterTest5
      COMMAND ITKStatisticsTestDriver itkSampleToHistogramFilterTest5
              DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png})
itk_add_test(NAME itkSampleToHistogramFilterTest6
      COMMAND ITKStatisticsTestDriver itkSampleToHistogramFilterTest6)
itk_add_test(NAME itkSampleToHistogramFilterTest7
      COMMAND ITKStatisticsTestDriver itkSampleToHistogramFilterTest7)
itk_add_test(NAME itkSampleToSubsampleFilterTest1
      COMMAND ITKStatisticsTestDriver itkSampleToSubsampleFilterTest1)
itk_add_test(NAME itkScalarImageToCooccurrenceListSampleFilterTest
      COMMAND ITKStatisticsTestDriver itkScalarImageToCooccurrenceListSampleFilterTest)
itk_add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest
      COMMAND ITKStatisticsTestDriver itkScalarImageToCooccurrenceMatrixFilterTest)
itk_add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest2
      COMMAND ITKStatisticsTestDriver itkScalarImageToCooccurrenceMatrixFilterTest2)
itk_add_test(NAME itkScalarImageToTextureFeaturesFilterTest
      COMMAND ITKStatisticsTestDriver itkScalarImageToTextureFeaturesFilterTest)
itk_add_test(NAME itkScalarImageToRunLengthMatrixFilterTest
      COMMAND ITKStatisticsTestDriver itkScalarImageToRunLengthMatrixFilterTest)
itk_add_test(NAME itkScalarImageToRunLengthFeaturesFilterTest
      COMMAND ITKStatisticsTestDriver itkScalarImageToRunLengthFeaturesFilterTest)
itk_add_test(NAME itkSparseFrequencyContainer2Test
      COMMAND ITKStatisticsTestDriver itkSparseFrequencyContainer2Test)
itk_add_test(NAME itkSpatialNeighborSubsamplerTest
      COMMAND ITKStatisticsTestDriver itkSpatialNeighborSubsamplerTest)
itk_add_test(NAME itkStandardDeviationPerComponentSampleFilterTest
      COMMAND ITKStatisticsTestDriver itkStandardDeviationPerComponentSampleFilterTest)
itk_add_test(NAME itkStatisticsTypesTest
      COMMAND ITKStatisticsTestDriver itkStatisticsTypesTest)
itk_add_test(NAME itkSubsampleTest
      COMMAND ITKStatisticsTestDriver itkSubsampleTest)
itk_add_test(NAME itkSubsampleTest2
      COMMAND ITKStatisticsTestDriver itkSubsampleTest2)
itk_add_test(NAME itkSubsampleTest3
      COMMAND ITKStatisticsTestDriver itkSubsampleTest3)
itk_add_test(NAME itkTDistributionTest
      COMMAND ITKStatisticsTestDriver itkTDistributionTest)
itk_add_test(NAME itkStatisticsAlgorithmTest
      COMMAND ITKStatisticsTestDriver itkStatisticsAlgorithmTest)
itk_add_test(NAME itkStatisticsAlgorithmTest2
      COMMAND ITKStatisticsTestDriver itkStatisticsAlgorithmTest2)
itk_add_test(NAME itkUniformRandomSpatialNeighborSubsamplerTest
      COMMAND ITKStatisticsTestDriver
    --compare DATA{Baseline/itkUniformRandomSubsamplingTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkUniformRandomSubsamplingTest.mha
    itkUniformRandomSpatialNeighborSubsamplerTest
     ${ITK_TEST_OUTPUT_DIR}/itkUniformRandomSubsamplingTest.mha)
itk_add_test(NAME itkVectorContainerToListSampleAdaptorTest
      COMMAND ITKStatisticsTestDriver itkVectorContainerToListSampleAdaptorTest)
itk_python_expression_add_test(NAME itkNormalVariateGeneratorPythonTest
EXPRESSION "itkNormalVariateGenerator = itk.NormalVariateGenerator.New()")
