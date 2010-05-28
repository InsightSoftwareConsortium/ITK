Find_PACKAGE(TCL)
MARK_AS_ADVANCED(TCL_TCLSH)

IF( NOT ITK_DISABLE_CXX_TESTING )
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${StatisticsRefactoring_SOURCE_DIR}/Source)
  ENDIF(TCL_TCLSH)

SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

SET(STATISTICS_TESTS ${EXECUTABLE_OUTPUT_PATH}/itkStatisticsTests)
SET(STATISTICS_HEADER_TESTS ${EXECUTABLE_OUTPUT_PATH}/itkStatisticsHeaderTest)

ADD_TEST(itkStatisticsHeaderTest ${STATISTICS_HEADER_TESTS})
ADD_TEST(itkStatisticsPrintTest ${STATISTICS_TESTS} itkStatisticsPrintTest)
ADD_TEST(itkStatisticsTypesTest ${STATISTICS_TESTS} itkStatisticsTypesTest)

ADD_TEST(itkCovarianceSampleFilterTest ${STATISTICS_TESTS} itkCovarianceSampleFilterTest)
ADD_TEST(itkCovarianceSampleFilterTest2 ${STATISTICS_TESTS} itkCovarianceSampleFilterTest2)
ADD_TEST(itkCovarianceSampleFilterTest3 ${STATISTICS_TESTS} itkCovarianceSampleFilterTest3)
ADD_TEST(itkHistogramToTextureFeaturesFilterTest ${STATISTICS_TESTS} itkHistogramToTextureFeaturesFilterTest)
ADD_TEST(itkHistogramTest ${STATISTICS_TESTS} itkHistogramTest)
ADD_TEST(itkHistogramToEntropyImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToEntropyImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToEntropyImageFilterTest1.mhd)
ADD_TEST(itkHistogramToEntropyImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToEntropyImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToEntropyImageFilterTest2.mhd)
ADD_TEST(itkHistogramToIntensityImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToIntensityImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToIntensityImageFilterTest1.mhd)
ADD_TEST(itkHistogramToIntensityImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToIntensityImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToIntensityImageFilterTest2.mhd)
ADD_TEST(itkHistogramToLogProbabilityImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToLogProbabilityImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToLogProbabilityImageFilterTest1.mhd)
ADD_TEST(itkHistogramToLogProbabilityImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToLogProbabilityImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToLogProbabilityImageFilterTest2.mhd)
ADD_TEST(itkHistogramToProbabilityImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToProbabilityImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToProbabilityImageFilterTest1.mhd)
ADD_TEST(itkHistogramToProbabilityImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToProbabilityImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToProbabilityImageFilterTest2.mhd)
ADD_TEST(itkImageToHistogramFilterTest ${STATISTICS_TESTS} itkImageToHistogramFilterTest)
ADD_TEST(itkImageToHistogramFilterTest2 ${STATISTICS_TESTS} itkImageToHistogramFilterTest2
   ${ITK_SOURCE_DIR}/Testing/Data/Input/VisibleWomanEyeSlice.png
   ${TEMP}/itkImageToHistogramFilterTest2.txt)
ADD_TEST(itkImageToHistogramFilterTest3 ${STATISTICS_TESTS} itkImageToHistogramFilterTest3
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkImageToHistogramFilterTest3.txt)
ADD_TEST(itkImageToListSampleFilterTest ${STATISTICS_TESTS} itkImageToListSampleFilterTest)
ADD_TEST(itkImageToListSampleFilterTest2 ${STATISTICS_TESTS} itkImageToListSampleFilterTest2)
ADD_TEST(itkImageToListSampleFilterTest3 ${STATISTICS_TESTS} itkImageToListSampleFilterTest3)
ADD_TEST(itkListSampleTest1 ${STATISTICS_TESTS} itkListSampleTest 1)
ADD_TEST(itkListSampleTest2 ${STATISTICS_TESTS} itkListSampleTest 5)
ADD_TEST(itkListSampleTest3 ${STATISTICS_TESTS} itkListSampleTest 10)
ADD_TEST(itkListSampleTest4 ${STATISTICS_TESTS} itkListSampleTest 100)
ADD_TEST(itkSampleToHistogramFilterTest ${STATISTICS_TESTS} itkSampleToHistogramFilterTest)
ADD_TEST(itkSampleToHistogramFilterTest2 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest2)
ADD_TEST(itkSampleToHistogramFilterTest3 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest3)
ADD_TEST(itkSampleToHistogramFilterTest4 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest4)
ADD_TEST(itkSampleToHistogramFilterTest5 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest5
   ${ITK_SOURCE_DIR}/Testing/Data/Input/VisibleWomanEyeSlice.png
   )
ADD_TEST(itkSampleToHistogramFilterTest6 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest6)
ADD_TEST(itkSampleToHistogramFilterTest7 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest7)
ADD_TEST(itkStandardDeviationPerComponentSampleFilterTest ${STATISTICS_TESTS} itkStandardDeviationPerComponentSampleFilterTest)
ADD_TEST(itkMeanSampleFilterTest ${STATISTICS_TESTS} itkMeanSampleFilterTest)
ADD_TEST(itkMeasurementVectorTraitsTest ${STATISTICS_TESTS} itkMeasurementVectorTraitsTest)
ADD_TEST(itkMembershipSampleTest1 ${STATISTICS_TESTS} itkMembershipSampleTest1)
ADD_TEST(itkMembershipSampleTest2 ${STATISTICS_TESTS} itkMembershipSampleTest2)
ADD_TEST(itkMembershipSampleTest3 ${STATISTICS_TESTS} itkMembershipSampleTest3)
ADD_TEST(itkMembershipSampleTest4 ${STATISTICS_TESTS} itkMembershipSampleTest4)
ADD_TEST(itkMixtureModelComponentBaseTest ${STATISTICS_TESTS} itkMixtureModelComponentBaseTest)
ADD_TEST(itkNeighborhoodSamplerTest1 ${STATISTICS_TESTS} itkNeighborhoodSamplerTest1)
ADD_TEST(itkNormalVariateGeneratorTest1 ${STATISTICS_TESTS} itkNormalVariateGeneratorTest1)
ADD_TEST(itkRandomVariateGeneratorBaseTest ${STATISTICS_TESTS} itkRandomVariateGeneratorBaseTest)
ADD_TEST(itkScalarImageToCooccurrenceMatrixFilterTest ${STATISTICS_TESTS} itkScalarImageToCooccurrenceMatrixFilterTest)
ADD_TEST(itkScalarImageToCooccurrenceMatrixFilterTest2 ${STATISTICS_TESTS} itkScalarImageToCooccurrenceMatrixFilterTest2)
ADD_TEST(itkScalarImageToCooccurrenceListSampleFilterTest ${STATISTICS_TESTS} itkScalarImageToCooccurrenceListSampleFilterTest)
ADD_TEST(itkSampleTest ${STATISTICS_TESTS} itkSampleTest)
ADD_TEST(itkSampleTest2 ${STATISTICS_TESTS} itkSampleTest2)
ADD_TEST(itkSampleTest3 ${STATISTICS_TESTS} itkSampleTest3)
ADD_TEST(itkSampleTest4 ${STATISTICS_TESTS} itkSampleTest4)
ADD_TEST(itkSampleClassifierFilterTest1 ${STATISTICS_TESTS} itkSampleClassifierFilterTest1)
ADD_TEST(itkSampleClassifierFilterTest2 ${STATISTICS_TESTS} itkSampleClassifierFilterTest2)
ADD_TEST(itkSampleClassifierFilterTest3 ${STATISTICS_TESTS} itkSampleClassifierFilterTest3)
ADD_TEST(itkSampleClassifierFilterTest4 ${STATISTICS_TESTS} itkSampleClassifierFilterTest4)
ADD_TEST(itkSampleClassifierFilterTest5 ${STATISTICS_TESTS} itkSampleClassifierFilterTest5)
ADD_TEST(itkSampleClassifierFilterTest6 ${STATISTICS_TESTS} itkSampleClassifierFilterTest6)
ADD_TEST(itkSampleClassifierFilterTest7 ${STATISTICS_TESTS} 
        itkSampleClassifierFilterTest7 
        ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat 
        ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussianTesting.dat )

ADD_TEST(itkSampleToSubsampleFilterTest1 ${STATISTICS_TESTS} itkSampleToSubsampleFilterTest1)
ADD_TEST(itkSubsampleTest ${STATISTICS_TESTS} itkSubsampleTest)
ADD_TEST(itkSubsampleTest2 ${STATISTICS_TESTS} itkSubsampleTest2)
ADD_TEST(itkSubsampleTest3 ${STATISTICS_TESTS} itkSubsampleTest3)
ADD_TEST(itkStatisticsAlgorithmTest ${STATISTICS_TESTS} itkStatisticsAlgorithmTest)
ADD_TEST(itkStatisticsAlgorithmTest2 ${STATISTICS_TESTS} itkStatisticsAlgorithmTest2)
ADD_TEST(itkWeightedMeanSampleFilterTest ${STATISTICS_TESTS} itkWeightedMeanSampleFilterTest)
ADD_TEST(itkWeightedCovarianceSampleFilterTest ${STATISTICS_TESTS} itkWeightedCovarianceSampleFilterTest)
ADD_TEST(itkImageToListSampleAdaptorTest ${STATISTICS_TESTS} itkImageToListSampleAdaptorTest)
ADD_TEST(itkImageToListSampleAdaptorTest2 ${STATISTICS_TESTS} itkImageToListSampleAdaptorTest2)
ADD_TEST(itkJointDomainImageToListSampleAdaptorTest ${STATISTICS_TESTS} itkJointDomainImageToListSampleAdaptorTest)
ADD_TEST(itkPointSetToListSampleAdaptorTest ${STATISTICS_TESTS} itkPointSetToListSampleAdaptorTest)
ADD_TEST(itkProbabilityDistributionTest ${STATISTICS_TESTS} itkProbabilityDistributionTest)
ADD_TEST(itkDenseFrequencyContainer2Test ${STATISTICS_TESTS} itkDenseFrequencyContainer2Test)
ADD_TEST(itkSparseFrequencyContainer2Test ${STATISTICS_TESTS} itkSparseFrequencyContainer2Test)
ADD_TEST(itkScalarImageToTextureFeaturesFilterTest ${STATISTICS_TESTS} itkScalarImageToTextureFeaturesFilterTest)


ADD_TEST(itkMembershipFunctionBaseTest ${STATISTICS_TESTS} itkMembershipFunctionBaseTest)
ADD_TEST(itkMembershipFunctionBaseTest2 ${STATISTICS_TESTS} itkMembershipFunctionBaseTest2)
ADD_TEST(itkDistanceToCentroidMembershipFunctionTest ${STATISTICS_TESTS} itkDistanceToCentroidMembershipFunctionTest)
ADD_TEST(itkGaussianMembershipFunctionTest ${STATISTICS_TESTS}
itkGaussianMembershipFunctionTest)
ADD_TEST(itkDistanceMetricTest ${STATISTICS_TESTS} itkDistanceMetricTest)
ADD_TEST(itkDistanceMetricTest2 ${STATISTICS_TESTS} itkDistanceMetricTest2)
ADD_TEST(itkEuclideanDistanceMetricTest ${STATISTICS_TESTS} itkEuclideanDistanceMetricTest)
ADD_TEST(itkEuclideanSquareDistanceMetricTest ${STATISTICS_TESTS} itkEuclideanSquareDistanceMetricTest)
ADD_TEST(itkMahalanobisDistanceMetricTest ${STATISTICS_TESTS} itkMahalanobisDistanceMetricTest)
ADD_TEST(itkManhattanDistanceMetricTest ${STATISTICS_TESTS} itkManhattanDistanceMetricTest)
ADD_TEST(itkDecisionRuleTest ${STATISTICS_TESTS} itkDecisionRuleTest)
ADD_TEST(itkMaximumDecisionRuleTest ${STATISTICS_TESTS} itkMaximumDecisionRuleTest)
ADD_TEST(itkMaximumRatioDecisionRuleTest ${STATISTICS_TESTS} itkMaximumRatioDecisionRuleTest)
ADD_TEST(itkMinimumDecisionRuleTest ${STATISTICS_TESTS} itkMinimumDecisionRuleTest)

ADD_TEST(itkKdTreeBasedKmeansEstimatorTest ${STATISTICS_TESTS} 
         itkKdTreeBasedKmeansEstimatorTest 
         ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat 
         1 28.54746 0.07)
ADD_TEST(itkKdTreeGeneratorTest ${STATISTICS_TESTS} 
         itkKdTreeGeneratorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkKdTreeTest1 ${STATISTICS_TESTS} itkKdTreeTest1 100 100 1 ${TEMP}/itkKdTreeTest1_100.dot)
ADD_TEST(itkKdTreeTestSamplePoints ${STATISTICS_TESTS} itkKdTreeTestSamplePoints )
ADD_TEST(itkKdTreeTest2 ${STATISTICS_TESTS} itkKdTreeTest2
         ${ITK_DATA_ROOT}/Input/pointsForKdTree.txt 1 ${TEMP}/itkKdTreeTest2.dot)
ADD_TEST(itkKdTreeTest3 ${STATISTICS_TESTS} itkKdTreeTest2
         ${ITK_DATA_ROOT}/Input/pointsForKdTree2.txt 1 ${TEMP}/itkKdTreeTest3.dot)
IF(DOXYGEN_DOT_EXECUTABLE)
ADD_TEST(itkKdTreeTest1Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest1_100.png ${TEMP}/itkKdTreeTest1_100.dot) 
SET_TESTS_PROPERTIES( itkKdTreeTest1Plot PROPERTIES DEPENDS itkKdTreeTest1 )
ADD_TEST(itkKdTreeTest2Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest2.png ${TEMP}/itkKdTreeTest2.dot) 
SET_TESTS_PROPERTIES( itkKdTreeTest2Plot PROPERTIES DEPENDS itkKdTreeTest2 )
ADD_TEST(itkKdTreeTest3Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest3.png ${TEMP}/itkKdTreeTest3.dot) 
SET_TESTS_PROPERTIES( itkKdTreeTest3Plot PROPERTIES DEPENDS itkKdTreeTest3 )
ENDIF(DOXYGEN_DOT_EXECUTABLE)
ADD_TEST(itkKdTreeTest4 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 1 )
ADD_TEST(itkKdTreeTest5 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 2 )
ADD_TEST(itkKdTreeTest6 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 3 )
ADD_TEST(itkKdTreeTest7 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 5 )
ADD_TEST(itkKdTreeTest7 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 10 )
ADD_TEST(itkKdTreeTest8 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 20 )
ADD_TEST(itkKdTreeTest9 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 100 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest1 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 1 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest2 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 2 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest3 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 3 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest4 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 5 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest5 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 10 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest6 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 20 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest7 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 100 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest8 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest8 1000 1000 1 )
ADD_TEST(itkWeightedCentroidKdTreeGeneratorTest9 ${STATISTICS_TESTS} 
  itkWeightedCentroidKdTreeGeneratorTest9 1000 1000 1 )


ADD_TEST(itkGaussianMixtureModelComponentTest 
         ${STATISTICS_TESTS} 
         itkGaussianMixtureModelComponentTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

ADD_TEST(itkExpectationMaximizationMixtureModelEstimatorTest 
         ${STATISTICS_TESTS} 
         itkExpectationMaximizationMixtureModelEstimatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

ADD_TEST(itkImageClassifierFilterTest 
         ${STATISTICS_TESTS} 
        --compare ${ITK_DATA_ROOT}/Baseline/Statistics/ImageClassifierFilterTestClassifiedImageTest.png
         ${TEMP}/ImageClassifierFilterTestClassifiedImage.png
         itkImageClassifierFilterTest
         ${TEMP}/ImageClassifierFilterTestClassifiedImage.png)

ADD_TEST(itkChiSquareDistributionTest ${STATISTICS_TESTS}
  itkChiSquareDistributionTest
  )
ADD_TEST(itkGaussianDistributionTest ${STATISTICS_TESTS}
  itkGaussianDistributionTest
  )
ADD_TEST(itkTDistributionTest ${STATISTICS_TESTS} itkTDistributionTest)


SET(Statistics_SRCS
  itkCovarianceSampleFilterTest.cxx
  itkCovarianceSampleFilterTest2.cxx
  itkCovarianceSampleFilterTest3.cxx
  itkHistogramTest.cxx
  itkHistogramToTextureFeaturesFilterTest.cxx
  itkHistogramToEntropyImageFilterTest1.cxx
  itkHistogramToEntropyImageFilterTest2.cxx
  itkHistogramToIntensityImageFilterTest1.cxx
  itkHistogramToIntensityImageFilterTest2.cxx
  itkHistogramToLogProbabilityImageFilterTest1.cxx
  itkHistogramToLogProbabilityImageFilterTest2.cxx
  itkHistogramToProbabilityImageFilterTest1.cxx
  itkHistogramToProbabilityImageFilterTest2.cxx
  itkImageToHistogramFilterTest.cxx
  itkImageToHistogramFilterTest2.cxx
  itkImageToHistogramFilterTest3.cxx
  itkImageToListSampleFilterTest.cxx
  itkImageToListSampleFilterTest2.cxx
  itkImageToListSampleFilterTest3.cxx
  itkListSampleTest.cxx
  itkSampleToHistogramFilterTest.cxx
  itkSampleToHistogramFilterTest2.cxx
  itkSampleToHistogramFilterTest3.cxx
  itkSampleToHistogramFilterTest4.cxx
  itkSampleToHistogramFilterTest5.cxx
  itkSampleToHistogramFilterTest6.cxx
  itkSampleToHistogramFilterTest7.cxx
  itkStandardDeviationPerComponentSampleFilterTest.cxx
  itkMeanSampleFilterTest.cxx
  itkMeasurementVectorTraitsTest.cxx
  itkMembershipSampleTest1.cxx
  itkMembershipSampleTest2.cxx
  itkMembershipSampleTest3.cxx
  itkMembershipSampleTest4.cxx
  itkMixtureModelComponentBaseTest.cxx
  itkNeighborhoodSamplerTest1.cxx
  itkNormalVariateGeneratorTest1.cxx
  itkRandomVariateGeneratorBaseTest.cxx
  itkScalarImageToCooccurrenceMatrixFilterTest.cxx
  itkScalarImageToCooccurrenceMatrixFilterTest2.cxx
  itkScalarImageToCooccurrenceListSampleFilterTest.cxx
  itkStatisticsPrintTest.cxx
  itkStatisticsTypesTest.cxx
  itkStatisticsAlgorithmTest.cxx
  itkStatisticsAlgorithmTest2.cxx
  itkSampleTest.cxx
  itkSampleTest2.cxx
  itkSampleTest3.cxx
  itkSampleTest4.cxx
  itkSampleClassifierFilterTest1.cxx
  itkSampleClassifierFilterTest2.cxx
  itkSampleClassifierFilterTest3.cxx
  itkSampleClassifierFilterTest4.cxx
  itkSampleClassifierFilterTest5.cxx
  itkSampleClassifierFilterTest6.cxx
  itkSampleClassifierFilterTest7.cxx
  itkSampleToSubsampleFilterTest1.cxx
  itkSubsampleTest.cxx
  itkSubsampleTest2.cxx
  itkSubsampleTest3.cxx
  itkWeightedMeanSampleFilterTest.cxx
  itkWeightedCovarianceSampleFilterTest.cxx
  itkImageToListSampleAdaptorTest.cxx
  itkImageToListSampleAdaptorTest2.cxx
  itkJointDomainImageToListSampleAdaptorTest.cxx
  itkPointSetToListSampleAdaptorTest.cxx
  itkProbabilityDistributionTest.cxx
  itkDenseFrequencyContainer2Test.cxx
  itkSparseFrequencyContainer2Test.cxx
  itkScalarImageToTextureFeaturesFilterTest.cxx


  itkMembershipFunctionBaseTest.cxx
  itkMembershipFunctionBaseTest2.cxx
  itkDistanceToCentroidMembershipFunctionTest.cxx
  itkGaussianMembershipFunctionTest.cxx
  itkDistanceMetricTest.cxx
  itkDistanceMetricTest2.cxx
  itkEuclideanDistanceMetricTest.cxx
  itkEuclideanSquareDistanceMetricTest.cxx
  itkMahalanobisDistanceMetricTest.cxx
  itkManhattanDistanceMetricTest.cxx
  itkDecisionRuleTest.cxx
  itkMaximumDecisionRuleTest.cxx
  itkMaximumRatioDecisionRuleTest.cxx
  itkMinimumDecisionRuleTest.cxx

  itkKdTreeBasedKmeansEstimatorTest.cxx
  itkKdTreeGeneratorTest.cxx
  itkKdTreeTest1.cxx
  itkKdTreeTestSamplePoints.cxx
  itkKdTreeTest2.cxx
  itkWeightedCentroidKdTreeGeneratorTest1.cxx
  itkWeightedCentroidKdTreeGeneratorTest8.cxx
  itkWeightedCentroidKdTreeGeneratorTest9.cxx

  itkGaussianMixtureModelComponentTest.cxx
  itkExpectationMaximizationMixtureModelEstimatorTest.cxx

  itkImageClassifierFilterTest.cxx

  itkGaussianDistributionTest.cxx
  itkChiSquareDistributionTest.cxx
  itkTDistributionTest.cxx
  )

ADD_EXECUTABLE(itkStatisticsTests itkStatisticsTests.cxx ${Statistics_SRCS})
TARGET_LINK_LIBRARIES(itkStatisticsTests ITKIO ITKCommon ITKNumerics ITKStatistics)

ADD_EXECUTABLE(itkStatisticsHeaderTest itkStatisticsHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkStatisticsHeaderTest ITKIO ITKCommon ITKNumerics  ITKStatistics)



ENDIF( NOT ITK_DISABLE_CXX_TESTING )


