IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

SET(STATISTICS_TESTS ${CXX_TEST_PATH}/itkStatisticsTests)

ADD_TEST(itkStatisticsAlgorithmTest ${STATISTICS_TESTS} itkStatisticsAlgorithmTest)
ADD_TEST(itkCovarianceCalculatorTest ${STATISTICS_TESTS} itkCovarianceCalculatorTest)
ADD_TEST(itkMeanCalculatorTest ${STATISTICS_TESTS} itkMeanCalculatorTest)
ADD_TEST(itkWeightedMeanCalculatorTest ${STATISTICS_TESTS} itkWeightedMeanCalculatorTest)
ADD_TEST(itkSubsampleTest ${STATISTICS_TESTS} itkSubsampleTest)
ADD_TEST(itkMembershipSampleTest ${STATISTICS_TESTS} itkMembershipSampleTest)
ADD_TEST(itkImageToListAdaptorTest ${STATISTICS_TESTS} itkImageToListAdaptorTest)
ADD_TEST(itkListSampleTest ${STATISTICS_TESTS} itkListSampleTest)
ADD_TEST(itkListSampleToHistogramGeneratorTest ${STATISTICS_TESTS} itkListSampleToHistogramGeneratorTest)
ADD_TEST(itkHistogramTest ${STATISTICS_TESTS} itkHistogramTest)
ADD_TEST(itkDenseFrequencyContainerTest ${STATISTICS_TESTS} itkDenseFrequencyContainerTest)
ADD_TEST(itkNormalVariateGeneratorTest ${STATISTICS_TESTS} 
                    itkNormalVariateGeneratorTest)
ADD_TEST(itkKdTreeBasedKmeansEstimatorTest ${STATISTICS_TESTS} 
         itkKdTreeBasedKmeansEstimatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkExpectationMaximizationMixtureModelEstimatorTest ${STATISTICS_TESTS} 
         itkExpectationMaximizationMixtureModelEstimatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

ADD_TEST(itkGoodnessOfFitMixtureModelCostFunctionTest ${STATISTICS_TESTS} 
         itkGoodnessOfFitMixtureModelCostFunctionTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

ADD_TEST(itkSampleClassifierTest ${STATISTICS_TESTS} 
         itkSampleClassifierTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

ADD_TEST(itkNeighborhoodSamplerTest ${STATISTICS_TESTS} itkNeighborhoodSamplerTest)

SET(Statistics_SRCS
itkStatisticsAlgorithmTest.cxx
itkCovarianceCalculatorTest.cxx
itkMeanCalculatorTest.cxx
itkWeightedMeanCalculatorTest.cxx
itkSubsampleTest.cxx
itkMembershipSampleTest.cxx
itkImageToListAdaptorTest.cxx
itkListSampleTest.cxx
itkListSampleToHistogramGeneratorTest.cxx
itkHistogramTest.cxx
itkDenseFrequencyContainerTest.cxx
itkNormalVariateGeneratorTest.cxx
itkKdTreeBasedKmeansEstimatorTest.cxx
itkExpectationMaximizationMixtureModelEstimatorTest.cxx
itkGoodnessOfFitMixtureModelCostFunctionTest
itkSampleClassifierTest.cxx
itkNeighborhoodSamplerTest.cxx
)

ADD_EXECUTABLE(itkStatisticsTests itkStatisticsTests.cxx ${Statistics_SRCS})
TARGET_LINK_LIBRARIES(itkStatisticsTests ITKIO ITKCommon ITKNumerics ITKStatistics)







