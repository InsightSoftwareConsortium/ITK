Find_PACKAGE(TCL)
mark_as_advanced(TCL_TCLSH)

if( NOT ITK_DISABLE_CXX_TESTING )
  if(TCL_TCLSH)
    add_test(PrintSelf-Statistics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${StatisticsRefactoring_SOURCE_DIR}/Source)
  endif(TCL_TCLSH)

set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

set(STATISTICS_TESTS ${EXECUTABLE_OUTPUT_PATH}/itkStatisticsTests)
set(STATISTICS_HEADER_TESTS ${EXECUTABLE_OUTPUT_PATH}/itkStatisticsHeaderTest)

add_test(itkStatisticsHeaderTest ${STATISTICS_HEADER_TESTS})
add_test(itkStatisticsPrintTest ${STATISTICS_TESTS} itkStatisticsPrintTest)
add_test(itkStatisticsTypesTest ${STATISTICS_TESTS} itkStatisticsTypesTest)

add_test(itkCovarianceSampleFilterTest ${STATISTICS_TESTS} itkCovarianceSampleFilterTest)
add_test(itkCovarianceSampleFilterTest2 ${STATISTICS_TESTS} itkCovarianceSampleFilterTest2)
add_test(itkCovarianceSampleFilterTest3 ${STATISTICS_TESTS} itkCovarianceSampleFilterTest3)
add_test(itkHistogramToTextureFeaturesFilterTest ${STATISTICS_TESTS} itkHistogramToTextureFeaturesFilterTest)
add_test(itkHistogramTest ${STATISTICS_TESTS} itkHistogramTest)
add_test(itkHistogramToEntropyImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToEntropyImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToEntropyImageFilterTest1.mhd)
add_test(itkHistogramToEntropyImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToEntropyImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToEntropyImageFilterTest2.mhd)
add_test(itkHistogramToIntensityImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToIntensityImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToIntensityImageFilterTest1.mhd)
add_test(itkHistogramToIntensityImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToIntensityImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToIntensityImageFilterTest2.mhd)
add_test(itkHistogramToLogProbabilityImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToLogProbabilityImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToLogProbabilityImageFilterTest1.mhd)
add_test(itkHistogramToLogProbabilityImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToLogProbabilityImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToLogProbabilityImageFilterTest2.mhd)
add_test(itkHistogramToProbabilityImageFilterTest1 ${STATISTICS_TESTS} itkHistogramToProbabilityImageFilterTest1
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkHistogramToProbabilityImageFilterTest1.mhd)
add_test(itkHistogramToProbabilityImageFilterTest2 ${STATISTICS_TESTS} itkHistogramToProbabilityImageFilterTest2
   ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
   ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
   ${TEMP}/itkHistogramToProbabilityImageFilterTest2.mhd)
add_test(itkImageToHistogramFilterTest ${STATISTICS_TESTS} itkImageToHistogramFilterTest)
add_test(itkImageToHistogramFilterTest2 ${STATISTICS_TESTS} itkImageToHistogramFilterTest2
   ${ITK_SOURCE_DIR}/Testing/Data/Input/VisibleWomanEyeSlice.png
   ${TEMP}/itkImageToHistogramFilterTest2.txt)
add_test(itkImageToHistogramFilterTest3 ${STATISTICS_TESTS} itkImageToHistogramFilterTest3
   ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
   ${TEMP}/itkImageToHistogramFilterTest3.txt)
add_test(itkImageToListSampleFilterTest ${STATISTICS_TESTS} itkImageToListSampleFilterTest)
add_test(itkImageToListSampleFilterTest2 ${STATISTICS_TESTS} itkImageToListSampleFilterTest2)
add_test(itkImageToListSampleFilterTest3 ${STATISTICS_TESTS} itkImageToListSampleFilterTest3)
add_test(itkListSampleTest1 ${STATISTICS_TESTS} itkListSampleTest 1)
add_test(itkListSampleTest2 ${STATISTICS_TESTS} itkListSampleTest 5)
add_test(itkListSampleTest3 ${STATISTICS_TESTS} itkListSampleTest 10)
add_test(itkListSampleTest4 ${STATISTICS_TESTS} itkListSampleTest 100)
add_test(itkSampleToHistogramFilterTest ${STATISTICS_TESTS} itkSampleToHistogramFilterTest)
add_test(itkSampleToHistogramFilterTest2 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest2)
add_test(itkSampleToHistogramFilterTest3 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest3)
add_test(itkSampleToHistogramFilterTest4 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest4)
add_test(itkSampleToHistogramFilterTest5 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest5
   ${ITK_SOURCE_DIR}/Testing/Data/Input/VisibleWomanEyeSlice.png
   )
add_test(itkSampleToHistogramFilterTest6 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest6)
add_test(itkSampleToHistogramFilterTest7 ${STATISTICS_TESTS} itkSampleToHistogramFilterTest7)
add_test(itkStandardDeviationPerComponentSampleFilterTest ${STATISTICS_TESTS} itkStandardDeviationPerComponentSampleFilterTest)
add_test(itkMeanSampleFilterTest ${STATISTICS_TESTS} itkMeanSampleFilterTest)
add_test(itkMeasurementVectorTraitsTest ${STATISTICS_TESTS} itkMeasurementVectorTraitsTest)
add_test(itkMembershipSampleTest1 ${STATISTICS_TESTS} itkMembershipSampleTest1)
add_test(itkMembershipSampleTest2 ${STATISTICS_TESTS} itkMembershipSampleTest2)
add_test(itkMembershipSampleTest3 ${STATISTICS_TESTS} itkMembershipSampleTest3)
add_test(itkMembershipSampleTest4 ${STATISTICS_TESTS} itkMembershipSampleTest4)
add_test(itkMixtureModelComponentBaseTest ${STATISTICS_TESTS} itkMixtureModelComponentBaseTest)
add_test(itkNeighborhoodSamplerTest1 ${STATISTICS_TESTS} itkNeighborhoodSamplerTest1)
add_test(itkNormalVariateGeneratorTest1 ${STATISTICS_TESTS} itkNormalVariateGeneratorTest1)
add_test(itkRandomVariateGeneratorBaseTest ${STATISTICS_TESTS} itkRandomVariateGeneratorBaseTest)
add_test(itkScalarImageToCooccurrenceMatrixFilterTest ${STATISTICS_TESTS} itkScalarImageToCooccurrenceMatrixFilterTest)
add_test(itkScalarImageToCooccurrenceMatrixFilterTest2 ${STATISTICS_TESTS} itkScalarImageToCooccurrenceMatrixFilterTest2)
add_test(itkScalarImageToCooccurrenceListSampleFilterTest ${STATISTICS_TESTS} itkScalarImageToCooccurrenceListSampleFilterTest)
add_test(itkSampleTest ${STATISTICS_TESTS} itkSampleTest)
add_test(itkSampleTest2 ${STATISTICS_TESTS} itkSampleTest2)
add_test(itkSampleTest3 ${STATISTICS_TESTS} itkSampleTest3)
add_test(itkSampleTest4 ${STATISTICS_TESTS} itkSampleTest4)
add_test(itkSampleClassifierFilterTest1 ${STATISTICS_TESTS} itkSampleClassifierFilterTest1)
add_test(itkSampleClassifierFilterTest2 ${STATISTICS_TESTS} itkSampleClassifierFilterTest2)
add_test(itkSampleClassifierFilterTest3 ${STATISTICS_TESTS} itkSampleClassifierFilterTest3)
add_test(itkSampleClassifierFilterTest4 ${STATISTICS_TESTS} itkSampleClassifierFilterTest4)
add_test(itkSampleClassifierFilterTest5 ${STATISTICS_TESTS} itkSampleClassifierFilterTest5)
add_test(itkSampleClassifierFilterTest6 ${STATISTICS_TESTS} itkSampleClassifierFilterTest6)
add_test(itkSampleClassifierFilterTest7 ${STATISTICS_TESTS}
        itkSampleClassifierFilterTest7
        ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat
        ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussianTesting.dat )

add_test(itkSampleToSubsampleFilterTest1 ${STATISTICS_TESTS} itkSampleToSubsampleFilterTest1)
add_test(itkSubsampleTest ${STATISTICS_TESTS} itkSubsampleTest)
add_test(itkSubsampleTest2 ${STATISTICS_TESTS} itkSubsampleTest2)
add_test(itkSubsampleTest3 ${STATISTICS_TESTS} itkSubsampleTest3)
add_test(itkStatisticsAlgorithmTest ${STATISTICS_TESTS} itkStatisticsAlgorithmTest)
add_test(itkStatisticsAlgorithmTest2 ${STATISTICS_TESTS} itkStatisticsAlgorithmTest2)
add_test(itkWeightedMeanSampleFilterTest ${STATISTICS_TESTS} itkWeightedMeanSampleFilterTest)
add_test(itkWeightedCovarianceSampleFilterTest ${STATISTICS_TESTS} itkWeightedCovarianceSampleFilterTest)
add_test(itkWeightedCovarianceSampleFilterTest2 ${STATISTICS_TESTS} itkWeightedCovarianceSampleFilterTest2)
add_test(itkImageToListSampleAdaptorTest ${STATISTICS_TESTS} itkImageToListSampleAdaptorTest)
add_test(itkImageToListSampleAdaptorTest2 ${STATISTICS_TESTS} itkImageToListSampleAdaptorTest2)
add_test(itkJointDomainImageToListSampleAdaptorTest ${STATISTICS_TESTS} itkJointDomainImageToListSampleAdaptorTest)
add_test(itkPointSetToListSampleAdaptorTest ${STATISTICS_TESTS} itkPointSetToListSampleAdaptorTest)
add_test(itkProbabilityDistributionTest ${STATISTICS_TESTS} itkProbabilityDistributionTest)
add_test(itkDenseFrequencyContainer2Test ${STATISTICS_TESTS} itkDenseFrequencyContainer2Test)
add_test(itkSparseFrequencyContainer2Test ${STATISTICS_TESTS} itkSparseFrequencyContainer2Test)
add_test(itkScalarImageToTextureFeaturesFilterTest ${STATISTICS_TESTS} itkScalarImageToTextureFeaturesFilterTest)


add_test(itkMembershipFunctionBaseTest ${STATISTICS_TESTS} itkMembershipFunctionBaseTest)
add_test(itkMembershipFunctionBaseTest2 ${STATISTICS_TESTS} itkMembershipFunctionBaseTest2)
add_test(itkDistanceToCentroidMembershipFunctionTest ${STATISTICS_TESTS} itkDistanceToCentroidMembershipFunctionTest)
add_test(itkGaussianMembershipFunctionTest ${STATISTICS_TESTS}
itkGaussianMembershipFunctionTest)
add_test(itkDistanceMetricTest ${STATISTICS_TESTS} itkDistanceMetricTest)
add_test(itkDistanceMetricTest2 ${STATISTICS_TESTS} itkDistanceMetricTest2)
add_test(itkEuclideanDistanceMetricTest ${STATISTICS_TESTS} itkEuclideanDistanceMetricTest)
add_test(itkEuclideanSquareDistanceMetricTest ${STATISTICS_TESTS} itkEuclideanSquareDistanceMetricTest)
add_test(itkMahalanobisDistanceMetricTest ${STATISTICS_TESTS} itkMahalanobisDistanceMetricTest)
add_test(itkManhattanDistanceMetricTest ${STATISTICS_TESTS} itkManhattanDistanceMetricTest)
add_test(itkDecisionRuleTest ${STATISTICS_TESTS} itkDecisionRuleTest)
add_test(itkMaximumDecisionRuleTest ${STATISTICS_TESTS} itkMaximumDecisionRuleTest)
add_test(itkMaximumRatioDecisionRuleTest ${STATISTICS_TESTS} itkMaximumRatioDecisionRuleTest)
add_test(itkMinimumDecisionRuleTest ${STATISTICS_TESTS} itkMinimumDecisionRuleTest)

add_test(itkKdTreeBasedKmeansEstimatorTest ${STATISTICS_TESTS}
         itkKdTreeBasedKmeansEstimatorTest
         ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat
         1 28.54746 0.07)
add_test(itkKdTreeGeneratorTest ${STATISTICS_TESTS}
         itkKdTreeGeneratorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(itkKdTreeTest1 ${STATISTICS_TESTS} itkKdTreeTest1 100 100 1 ${TEMP}/itkKdTreeTest1_100.dot)
add_test(itkKdTreeTestSamplePoints ${STATISTICS_TESTS} itkKdTreeTestSamplePoints )
add_test(itkKdTreeTest2 ${STATISTICS_TESTS} itkKdTreeTest2
         ${ITK_DATA_ROOT}/Input/pointsForKdTree.txt 1 ${TEMP}/itkKdTreeTest2.dot)
add_test(itkKdTreeTest3 ${STATISTICS_TESTS} itkKdTreeTest2
         ${ITK_DATA_ROOT}/Input/pointsForKdTree2.txt 1 ${TEMP}/itkKdTreeTest3.dot)
if(DOXYGEN_DOT_EXECUTABLE)
add_test(itkKdTreeTest1Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest1_100.png ${TEMP}/itkKdTreeTest1_100.dot)
set_tests_properties( itkKdTreeTest1Plot PROPERTIES DEPENDS itkKdTreeTest1 )
add_test(itkKdTreeTest2Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest2.png ${TEMP}/itkKdTreeTest2.dot)
set_tests_properties( itkKdTreeTest2Plot PROPERTIES DEPENDS itkKdTreeTest2 )
add_test(itkKdTreeTest3Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest3.png ${TEMP}/itkKdTreeTest3.dot)
set_tests_properties( itkKdTreeTest3Plot PROPERTIES DEPENDS itkKdTreeTest3 )
endif(DOXYGEN_DOT_EXECUTABLE)
add_test(itkKdTreeTest4 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 1 )
add_test(itkKdTreeTest5 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 2 )
add_test(itkKdTreeTest6 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 3 )
add_test(itkKdTreeTest7 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 5 )
add_test(itkKdTreeTest7 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 10 )
add_test(itkKdTreeTest8 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 20 )
add_test(itkKdTreeTest9 ${STATISTICS_TESTS} itkKdTreeTest1 1000 1000 100 )
add_test(itkWeightedCentroidKdTreeGeneratorTest1 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 1 )
add_test(itkWeightedCentroidKdTreeGeneratorTest2 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 2 )
add_test(itkWeightedCentroidKdTreeGeneratorTest3 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 3 )
add_test(itkWeightedCentroidKdTreeGeneratorTest4 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 5 )
add_test(itkWeightedCentroidKdTreeGeneratorTest5 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 10 )
add_test(itkWeightedCentroidKdTreeGeneratorTest6 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 20 )
add_test(itkWeightedCentroidKdTreeGeneratorTest7 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 100 )
add_test(itkWeightedCentroidKdTreeGeneratorTest8 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest8 1000 1000 1 )
add_test(itkWeightedCentroidKdTreeGeneratorTest9 ${STATISTICS_TESTS}
  itkWeightedCentroidKdTreeGeneratorTest9 1000 1000 1 )


add_test(itkGaussianMixtureModelComponentTest
         ${STATISTICS_TESTS}
         itkGaussianMixtureModelComponentTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

add_test(itkExpectationMaximizationMixtureModelEstimatorTest
         ${STATISTICS_TESTS}
         itkExpectationMaximizationMixtureModelEstimatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

add_test(itkImageClassifierFilterTest
         ${STATISTICS_TESTS}
        --compare ${ITK_DATA_ROOT}/Baseline/Statistics/ImageClassifierFilterTestClassifiedImageTest.png
         ${TEMP}/ImageClassifierFilterTestClassifiedImage.png
         itkImageClassifierFilterTest
         ${TEMP}/ImageClassifierFilterTestClassifiedImage.png)

add_test(itkChiSquareDistributionTest ${STATISTICS_TESTS}
  itkChiSquareDistributionTest
  )
add_test(itkGaussianDistributionTest ${STATISTICS_TESTS}
  itkGaussianDistributionTest
  )
add_test(itkTDistributionTest ${STATISTICS_TESTS} itkTDistributionTest)


set(Statistics_SRCS
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
  itkWeightedCovarianceSampleFilterTest2.cxx
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

add_executable(itkStatisticsTests itkStatisticsTests.cxx ${Statistics_SRCS})
target_link_libraries(itkStatisticsTests ITKIO ITKCommon ITKNumerics ITKStatistics)

add_executable(itkStatisticsHeaderTest itkStatisticsHeaderTest.cxx)
target_link_libraries(itkStatisticsHeaderTest ITKIO ITKCommon ITKNumerics  ITKStatistics)



endif( NOT ITK_DISABLE_CXX_TESTING )
