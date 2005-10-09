IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)




SET(STATISTICS_TESTS ${CXX_TEST_PATH}/itkStatisticsTests)

ADD_TEST(itkStatisticsAlgorithmTest ${STATISTICS_TESTS} itkStatisticsAlgorithmTest)
ADD_TEST(itkStatisticsPrintTest ${STATISTICS_TESTS} itkStatisticsPrintTest)
ADD_TEST(itkCovarianceCalculatorTest ${STATISTICS_TESTS} itkCovarianceCalculatorTest)
ADD_TEST(itkDenseFrequencyContainerTest ${STATISTICS_TESTS} itkDenseFrequencyContainerTest)
ADD_TEST(itkGaussianDensityFunctionTest ${STATISTICS_TESTS} itkGaussianDensityFunctionTest)
ADD_TEST(itkExpectationMaximizationMixtureModelEstimatorTest 
         ${STATISTICS_TESTS} 
         itkExpectationMaximizationMixtureModelEstimatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkGoodnessOfFitMixtureModelCostFunctionTest ${STATISTICS_TESTS} 
         itkGoodnessOfFitMixtureModelCostFunctionTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculatorTest ${STATISTICS_TESTS} 
         itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkHistogramTest ${STATISTICS_TESTS} itkHistogramTest)
ADD_TEST(itkImageToListAdaptorTest ${STATISTICS_TESTS} itkImageToListAdaptorTest)
ADD_TEST(itkImageToCooccurrenceListAdaptorTest ${STATISTICS_TESTS} 
         itkImageToCooccurrenceListAdaptorTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd)
ADD_TEST(itkImageToHistogramGeneratorTest ${STATISTICS_TESTS} itkImageToHistogramGeneratorTest)
ADD_TEST(itkKdTreeBasedKmeansEstimatorTest ${STATISTICS_TESTS} 
         itkKdTreeBasedKmeansEstimatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkKdTreeGeneratorTest ${STATISTICS_TESTS} 
         itkKdTreeGeneratorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkListSampleTest ${STATISTICS_TESTS} itkListSampleTest 4)
ADD_TEST(itkListSampleToHistogramFilterTest ${STATISTICS_TESTS} itkListSampleToHistogramFilterTest)
ADD_TEST(itkListSampleToHistogramGeneratorTest ${STATISTICS_TESTS} itkListSampleToHistogramGeneratorTest)
ADD_TEST(itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest ${STATISTICS_TESTS} 
         itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest)
ADD_TEST(itkMeanCalculatorTest ${STATISTICS_TESTS} itkMeanCalculatorTest)
ADD_TEST(itkMembershipSampleTest ${STATISTICS_TESTS} itkMembershipSampleTest)
ADD_TEST(itkNeighborhoodSamplerTest ${STATISTICS_TESTS} itkNeighborhoodSamplerTest)
ADD_TEST(itkMembershipSampleGeneratorTest ${STATISTICS_TESTS} itkMembershipSampleGeneratorTest)
ADD_TEST(itkMersenneTwisterRandomVariateGeneratorTest.cxx ${STATISTICS_TESTS}
                    itkMersenneTwisterRandomVariateGeneratorTest) 
ADD_TEST(itkNormalVariateGeneratorTest ${STATISTICS_TESTS} 
                    itkNormalVariateGeneratorTest)

ADD_TEST(itkSampleClassifierTest ${STATISTICS_TESTS} 
         itkSampleClassifierTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkSampleClassifierWithMaskTest ${STATISTICS_TESTS} 
         itkSampleClassifierWithMaskTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkSampleMeanShiftClusteringFilterTest ${STATISTICS_TESTS}                itkSampleMeanShiftClusteringFilterTest ${ITK_DATA_ROOT}/Input/sf4.png)
ADD_TEST(itkSampleSelectiveMeanShiftBlurringFilterTest ${STATISTICS_TESTS}                itkSampleSelectiveMeanShiftBlurringFilterTest ${ITK_DATA_ROOT}/Input/sf4.png)
ADD_TEST(itkScalarImageTextureCalculatorTest ${STATISTICS_TESTS} itkScalarImageTextureCalculatorTest)
ADD_TEST(itkScalarImageToHistogramGeneratorTest ${STATISTICS_TESTS} itkScalarImageToHistogramGeneratorTest)
ADD_TEST(itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest ${STATISTICS_TESTS} itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest)
ADD_TEST(itkSelectiveSubsampleGeneratorTest ${STATISTICS_TESTS} 
                                            itkSelectiveSubsampleGeneratorTest)
ADD_TEST(itkSubsampleTest ${STATISTICS_TESTS} itkSubsampleTest)
ADD_TEST(itkVariableDimensionHistogramTest ${STATISTICS_TESTS} itkVariableDimensionHistogramTest)
ADD_TEST(itkWeightedMeanCalculatorTest ${STATISTICS_TESTS} itkWeightedMeanCalculatorTest)
ADD_TEST(itkWeightedCovarianceCalculatorTest ${STATISTICS_TESTS} 
         itkWeightedCovarianceCalculatorTest)


SET(Statistics_SRCS
itkStatisticsPrintTest.cxx
itkStatisticsAlgorithmTest.cxx
itkCovarianceCalculatorTest.cxx
itkDenseFrequencyContainerTest.cxx
itkExpectationMaximizationMixtureModelEstimatorTest.cxx
itkGaussianDensityFunctionTest.cxx
itkGoodnessOfFitMixtureModelCostFunctionTest.cxx
itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculatorTest.cxx 
itkHistogramTest.cxx
itkImageToListAdaptorTest.cxx
itkImageToCooccurrenceListAdaptorTest.cxx
itkImageToHistogramGeneratorTest.cxx
itkKdTreeBasedKmeansEstimatorTest.cxx
itkKdTreeGeneratorTest.cxx
itkListSampleTest.cxx
itkListSampleToHistogramFilterTest.cxx
itkListSampleToHistogramGeneratorTest.cxx
itkMeanCalculatorTest.cxx
itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest.cxx
itkMembershipSampleTest.cxx
itkMembershipSampleGeneratorTest.cxx
itkMersenneTwisterRandomVariateGeneratorTest.cxx 
itkNeighborhoodSamplerTest.cxx
itkNormalVariateGeneratorTest.cxx
itkSampleClassifierTest.cxx
itkSampleClassifierWithMaskTest.cxx
itkSampleMeanShiftClusteringFilterTest.cxx
itkSampleSelectiveMeanShiftBlurringFilterTest.cxx
itkSelectiveSubsampleGeneratorTest.cxx
itkScalarImageToHistogramGeneratorTest.cxx
itkScalarImageTextureCalculatorTest.cxx
itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest.cxx
itkSubsampleTest.cxx
itkVariableDimensionHistogramTest.cxx
itkWeightedMeanCalculatorTest.cxx
itkWeightedCovarianceCalculatorTest.cxx
)

ADD_EXECUTABLE(itkStatisticsTests itkStatisticsTests.cxx ${Statistics_SRCS})
TARGET_LINK_LIBRARIES(itkStatisticsTests ITKIO ITKCommon ITKNumerics ITKStatistics)



ENDIF( NOT ITK_DISABLE_CXX_TESTING )


