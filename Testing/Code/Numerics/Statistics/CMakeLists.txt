Find_PACKAGE(TCL)
mark_as_advanced(TCL_TCLSH)

if(TCL_TCLSH)
  add_test(PrintSelf-Statistics ${TCL_TCLSH}
           ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
           ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
endif(TCL_TCLSH)

if( NOT ITK_DISABLE_CXX_TESTING )
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

set(KIT Statistics)
set(KIT_LIBS ITKIO ITKCommon ITKNumerics  ITKStatistics)
set(KitTests
  itkChiSquareDistributionTest
  itkCovarianceSampleFilterTest
  itkCovarianceSampleFilterTest2
  itkCovarianceSampleFilterTest3
  itkDecisionRuleTest
  itkDenseFrequencyContainer2Test
  itkDistanceMetricTest
  itkDistanceMetricTest2
  itkDistanceToCentroidMembershipFunctionTest
  itkEuclideanDistanceMetricTest
  itkEuclideanSquareDistanceMetricTest
  itkExpectationMaximizationMixtureModelEstimatorTest
  itkGaussianDistributionTest
  itkGaussianMembershipFunctionTest
  itkGaussianMixtureModelComponentTest
  itkHistogramTest
  itkHistogramToEntropyImageFilterTest1
  itkHistogramToEntropyImageFilterTest2
  itkHistogramToIntensityImageFilterTest1
  itkHistogramToIntensityImageFilterTest2
  itkHistogramToLogProbabilityImageFilterTest1
  itkHistogramToLogProbabilityImageFilterTest2
  itkHistogramToProbabilityImageFilterTest1
  itkHistogramToProbabilityImageFilterTest2
  itkHistogramToTextureFeaturesFilterTest
  itkImageClassifierFilterTest
  itkImageToHistogramFilterTest
  itkImageToHistogramFilterTest2
  itkImageToHistogramFilterTest3
  itkImageToListSampleAdaptorTest
  itkImageToListSampleAdaptorTest2
  itkImageToListSampleFilterTest
  itkImageToListSampleFilterTest2
  itkImageToListSampleFilterTest3
  itkJointDomainImageToListSampleAdaptorTest
  itkKdTreeBasedKmeansEstimatorTest
  itkKdTreeGeneratorTest
  itkKdTreeTest1
  itkKdTreeTest2
  itkKdTreeTestSamplePoints
  itkListSampleTest
  itkMahalanobisDistanceMetricTest
  itkManhattanDistanceMetricTest
  itkMaximumDecisionRuleTest
  itkMaximumRatioDecisionRuleTest
  itkMeanSampleFilterTest
  itkMeanSampleFilterTest2
  itkMeanSampleFilterTest3
  itkMeasurementVectorTraitsTest
  itkMembershipFunctionBaseTest
  itkMembershipFunctionBaseTest2
  itkMembershipSampleTest1
  itkMembershipSampleTest2
  itkMembershipSampleTest3
  itkMembershipSampleTest4
  itkMinimumDecisionRuleTest
  itkMixtureModelComponentBaseTest
  itkNeighborhoodSamplerTest1
  itkNormalVariateGeneratorTest1
  itkPointSetToListSampleAdaptorTest
  itkProbabilityDistributionTest
  itkRandomVariateGeneratorBaseTest
  itkSampleClassifierFilterTest1
  itkSampleClassifierFilterTest2
  itkSampleClassifierFilterTest3
  itkSampleClassifierFilterTest4
  itkSampleClassifierFilterTest5
  itkSampleClassifierFilterTest6
  itkSampleClassifierFilterTest7
  itkSampleTest
  itkSampleTest2
  itkSampleTest3
  itkSampleTest4
  itkSampleToHistogramFilterTest
  itkSampleToHistogramFilterTest2
  itkSampleToHistogramFilterTest3
  itkSampleToHistogramFilterTest4
  itkSampleToHistogramFilterTest5
  itkSampleToHistogramFilterTest6
  itkSampleToHistogramFilterTest7
  itkSampleToSubsampleFilterTest1
  itkScalarImageToCooccurrenceListSampleFilterTest
  itkScalarImageToCooccurrenceMatrixFilterTest
  itkScalarImageToCooccurrenceMatrixFilterTest2
  itkScalarImageToTextureFeaturesFilterTest
  itkSparseFrequencyContainer2Test
  itkStandardDeviationPerComponentSampleFilterTest
  itkStatisticsAlgorithmTest
  itkStatisticsAlgorithmTest2
  itkStatisticsPrintTest
  itkStatisticsTypesTest
  itkSubsampleTest
  itkSubsampleTest2
  itkSubsampleTest3
  itkTDistributionTest
  itkWeightedCentroidKdTreeGeneratorTest1
  itkWeightedCentroidKdTreeGeneratorTest8
  itkWeightedCentroidKdTreeGeneratorTest9
  itkWeightedCovarianceSampleFilterTest
  itkWeightedCovarianceSampleFilterTest2
  itkWeightedMeanSampleFilterTest
  )

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

set(STATISTICS_HEADER_TESTS ${EXECUTABLE_OUTPUT_PATH}/itkStatisticsHeaderTest)
add_executable(itkStatisticsHeaderTest itkStatisticsHeaderTest.cxx)
target_link_libraries(itkStatisticsHeaderTest ${KIT_LIBS})

add_test(itkStatisticsHeaderTest ${STATISTICS_HEADER_TESTS})
add_test(itkStatisticsPrintTest ${TestDriver} itkStatisticsPrintTest)
add_test(itkStatisticsTypesTest ${TestDriver} itkStatisticsTypesTest)

add_test(itkCovarianceSampleFilterTest ${TestDriver}
         itkCovarianceSampleFilterTest)
add_test(itkCovarianceSampleFilterTest2 ${TestDriver}
         itkCovarianceSampleFilterTest2)
add_test(itkCovarianceSampleFilterTest3 ${TestDriver}
         itkCovarianceSampleFilterTest3)
add_test(itkHistogramToTextureFeaturesFilterTest ${TestDriver}
         itkHistogramToTextureFeaturesFilterTest)
add_test(itkHistogramTest ${TestDriver}
         itkHistogramTest)
add_test(itkHistogramToEntropyImageFilterTest1 ${TestDriver}
         itkHistogramToEntropyImageFilterTest1
         ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
         ${TEMP}/itkHistogramToEntropyImageFilterTest1.mhd)
add_test(itkHistogramToEntropyImageFilterTest2 ${TestDriver}
         itkHistogramToEntropyImageFilterTest2
         ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
         ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
         ${TEMP}/itkHistogramToEntropyImageFilterTest2.mhd)
add_test(itkHistogramToIntensityImageFilterTest1 ${TestDriver}
         itkHistogramToIntensityImageFilterTest1
         ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
         ${TEMP}/itkHistogramToIntensityImageFilterTest1.mhd)
add_test(itkHistogramToIntensityImageFilterTest2 ${TestDriver}
         itkHistogramToIntensityImageFilterTest2
         ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
         ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
         ${TEMP}/itkHistogramToIntensityImageFilterTest2.mhd)
add_test(itkHistogramToLogProbabilityImageFilterTest1 ${TestDriver}
         itkHistogramToLogProbabilityImageFilterTest1
         ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
         ${TEMP}/itkHistogramToLogProbabilityImageFilterTest1.mhd)
add_test(itkHistogramToLogProbabilityImageFilterTest2 ${TestDriver}
         itkHistogramToLogProbabilityImageFilterTest2
         ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
         ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
         ${TEMP}/itkHistogramToLogProbabilityImageFilterTest2.mhd)
add_test(itkHistogramToProbabilityImageFilterTest1 ${TestDriver}
         itkHistogramToProbabilityImageFilterTest1
         ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
         ${TEMP}/itkHistogramToProbabilityImageFilterTest1.mhd)
add_test(itkHistogramToProbabilityImageFilterTest2 ${TestDriver}
         itkHistogramToProbabilityImageFilterTest2
         ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
         ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
         ${TEMP}/itkHistogramToProbabilityImageFilterTest2.mhd)
add_test(itkImageToHistogramFilterTest ${TestDriver}
         itkImageToHistogramFilterTest)
add_test(itkImageToHistogramFilterTest2 ${TestDriver}
         itkImageToHistogramFilterTest2
         ${ITK_SOURCE_DIR}/Testing/Data/Input/VisibleWomanEyeSlice.png
         ${TEMP}/itkImageToHistogramFilterTest2.txt)
add_test(itkImageToHistogramFilterTest3 ${TestDriver}
         itkImageToHistogramFilterTest3
         ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
         ${TEMP}/itkImageToHistogramFilterTest3.txt)
add_test(itkImageToListSampleFilterTest ${TestDriver}
         itkImageToListSampleFilterTest)
add_test(itkImageToListSampleFilterTest2 ${TestDriver}
         itkImageToListSampleFilterTest2)
add_test(itkImageToListSampleFilterTest3 ${TestDriver}
         itkImageToListSampleFilterTest3)
add_test(itkListSampleTest1 ${TestDriver}
         itkListSampleTest 1)
add_test(itkListSampleTest2 ${TestDriver}
         itkListSampleTest 5)
add_test(itkListSampleTest3 ${TestDriver}
         itkListSampleTest 10)
add_test(itkListSampleTest4 ${TestDriver}
         itkListSampleTest 100)
add_test(itkSampleToHistogramFilterTest ${TestDriver}
         itkSampleToHistogramFilterTest)
add_test(itkSampleToHistogramFilterTest2 ${TestDriver}
         itkSampleToHistogramFilterTest2)
add_test(itkSampleToHistogramFilterTest3 ${TestDriver}
         itkSampleToHistogramFilterTest3)
add_test(itkSampleToHistogramFilterTest4 ${TestDriver}
         itkSampleToHistogramFilterTest4)
add_test(itkSampleToHistogramFilterTest5 ${TestDriver}
         itkSampleToHistogramFilterTest5
         ${ITK_SOURCE_DIR}/Testing/Data/Input/VisibleWomanEyeSlice.png)
add_test(itkSampleToHistogramFilterTest6 ${TestDriver}
         itkSampleToHistogramFilterTest6)
add_test(itkSampleToHistogramFilterTest7 ${TestDriver}
         itkSampleToHistogramFilterTest7)
add_test(itkStandardDeviationPerComponentSampleFilterTest ${TestDriver}
         itkStandardDeviationPerComponentSampleFilterTest)
add_test(itkMeanSampleFilterTest ${TestDriver}
         itkMeanSampleFilterTest)
add_test(itkMeanSampleFilterTest2 ${TestDriver}
         itkMeanSampleFilterTest2)
add_test(itkMeanSampleFilterTest3 ${TestDriver}
         itkMeanSampleFilterTest3)
add_test(itkMeasurementVectorTraitsTest ${TestDriver}
         itkMeasurementVectorTraitsTest)
add_test(itkMembershipSampleTest1 ${TestDriver}
         itkMembershipSampleTest1)
add_test(itkMembershipSampleTest2 ${TestDriver}
         itkMembershipSampleTest2)
add_test(itkMembershipSampleTest3 ${TestDriver}
         itkMembershipSampleTest3)
add_test(itkMembershipSampleTest4 ${TestDriver}
         itkMembershipSampleTest4)
add_test(itkMixtureModelComponentBaseTest ${TestDriver}
         itkMixtureModelComponentBaseTest)
add_test(itkNeighborhoodSamplerTest1 ${TestDriver}
         itkNeighborhoodSamplerTest1)
add_test(itkNormalVariateGeneratorTest1 ${TestDriver}
         itkNormalVariateGeneratorTest1)
add_test(itkRandomVariateGeneratorBaseTest ${TestDriver}
         itkRandomVariateGeneratorBaseTest)
add_test(itkScalarImageToCooccurrenceMatrixFilterTest ${TestDriver}
         itkScalarImageToCooccurrenceMatrixFilterTest)
add_test(itkScalarImageToCooccurrenceMatrixFilterTest2 ${TestDriver}
         itkScalarImageToCooccurrenceMatrixFilterTest2)
add_test(itkScalarImageToCooccurrenceListSampleFilterTest ${TestDriver}
         itkScalarImageToCooccurrenceListSampleFilterTest)
add_test(itkSampleTest ${TestDriver}
         itkSampleTest)
add_test(itkSampleTest2 ${TestDriver}
         itkSampleTest2)
add_test(itkSampleTest3 ${TestDriver}
         itkSampleTest3)
add_test(itkSampleTest4 ${TestDriver}
         itkSampleTest4)
add_test(itkSampleClassifierFilterTest1 ${TestDriver}
         itkSampleClassifierFilterTest1)
add_test(itkSampleClassifierFilterTest2 ${TestDriver}
         itkSampleClassifierFilterTest2)
add_test(itkSampleClassifierFilterTest3 ${TestDriver}
         itkSampleClassifierFilterTest3)
add_test(itkSampleClassifierFilterTest4 ${TestDriver}
         itkSampleClassifierFilterTest4)
add_test(itkSampleClassifierFilterTest5 ${TestDriver}
         itkSampleClassifierFilterTest5)
add_test(itkSampleClassifierFilterTest6 ${TestDriver}
         itkSampleClassifierFilterTest6)
add_test(itkSampleClassifierFilterTest7 ${TestDriver}
         itkSampleClassifierFilterTest7
         ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat
         ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussianTesting.dat )

add_test(itkSampleToSubsampleFilterTest1 ${TestDriver}
         itkSampleToSubsampleFilterTest1)
add_test(itkSubsampleTest ${TestDriver}
         itkSubsampleTest)
add_test(itkSubsampleTest2 ${TestDriver}
         itkSubsampleTest2)
add_test(itkSubsampleTest3 ${TestDriver}
         itkSubsampleTest3)
add_test(itkStatisticsAlgorithmTest ${TestDriver}
         itkStatisticsAlgorithmTest)
add_test(itkStatisticsAlgorithmTest2 ${TestDriver}
         itkStatisticsAlgorithmTest2)
add_test(itkWeightedMeanSampleFilterTest ${TestDriver}
         itkWeightedMeanSampleFilterTest)
add_test(itkWeightedCovarianceSampleFilterTest ${TestDriver}
         itkWeightedCovarianceSampleFilterTest)
add_test(itkWeightedCovarianceSampleFilterTest2 ${TestDriver}
         itkWeightedCovarianceSampleFilterTest2)
add_test(itkImageToListSampleAdaptorTest ${TestDriver}
         itkImageToListSampleAdaptorTest)
add_test(itkImageToListSampleAdaptorTest2 ${TestDriver}
         itkImageToListSampleAdaptorTest2)
add_test(itkJointDomainImageToListSampleAdaptorTest ${TestDriver}
         itkJointDomainImageToListSampleAdaptorTest)
add_test(itkPointSetToListSampleAdaptorTest ${TestDriver}
         itkPointSetToListSampleAdaptorTest)
add_test(itkProbabilityDistributionTest ${TestDriver}
         itkProbabilityDistributionTest)
add_test(itkDenseFrequencyContainer2Test ${TestDriver}
         itkDenseFrequencyContainer2Test)
add_test(itkSparseFrequencyContainer2Test ${TestDriver}
         itkSparseFrequencyContainer2Test)
add_test(itkScalarImageToTextureFeaturesFilterTest ${TestDriver} itkScalarImageToTextureFeaturesFilterTest)


add_test(itkMembershipFunctionBaseTest ${TestDriver} itkMembershipFunctionBaseTest)
add_test(itkMembershipFunctionBaseTest2 ${TestDriver} itkMembershipFunctionBaseTest2)
add_test(itkDistanceToCentroidMembershipFunctionTest ${TestDriver} itkDistanceToCentroidMembershipFunctionTest)
add_test(itkGaussianMembershipFunctionTest ${TestDriver}
itkGaussianMembershipFunctionTest)
add_test(itkDistanceMetricTest ${TestDriver} itkDistanceMetricTest)
add_test(itkDistanceMetricTest2 ${TestDriver} itkDistanceMetricTest2)
add_test(itkEuclideanDistanceMetricTest ${TestDriver} itkEuclideanDistanceMetricTest)
add_test(itkEuclideanSquareDistanceMetricTest ${TestDriver} itkEuclideanSquareDistanceMetricTest)
add_test(itkMahalanobisDistanceMetricTest ${TestDriver} itkMahalanobisDistanceMetricTest)
add_test(itkManhattanDistanceMetricTest ${TestDriver} itkManhattanDistanceMetricTest)
add_test(itkDecisionRuleTest ${TestDriver} itkDecisionRuleTest)
add_test(itkMaximumDecisionRuleTest ${TestDriver} itkMaximumDecisionRuleTest)
add_test(itkMaximumRatioDecisionRuleTest ${TestDriver} itkMaximumRatioDecisionRuleTest)
add_test(itkMinimumDecisionRuleTest ${TestDriver} itkMinimumDecisionRuleTest)

add_test(itkKdTreeBasedKmeansEstimatorTest ${TestDriver}
         itkKdTreeBasedKmeansEstimatorTest
         ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat
         1 28.54746 0.07)
add_test(itkKdTreeGeneratorTest ${TestDriver}
         itkKdTreeGeneratorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(itkKdTreeTest1 ${TestDriver} itkKdTreeTest1 100 100 1 ${TEMP}/itkKdTreeTest1_100.dot)
add_test(itkKdTreeTestSamplePoints ${TestDriver} itkKdTreeTestSamplePoints )
add_test(itkKdTreeTest2 ${TestDriver} itkKdTreeTest2
         ${ITK_DATA_ROOT}/Input/pointsForKdTree.txt 1 ${TEMP}/itkKdTreeTest2.dot)
add_test(itkKdTreeTest3 ${TestDriver} itkKdTreeTest2
         ${ITK_DATA_ROOT}/Input/pointsForKdTree2.txt 1 ${TEMP}/itkKdTreeTest3.dot)
if(DOXYGEN_DOT_EXECUTABLE)
add_test(itkKdTreeTest1Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest1_100.png ${TEMP}/itkKdTreeTest1_100.dot)
set_tests_properties( itkKdTreeTest1Plot PROPERTIES DEPENDS itkKdTreeTest1 )
add_test(itkKdTreeTest2Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest2.png ${TEMP}/itkKdTreeTest2.dot)
set_tests_properties( itkKdTreeTest2Plot PROPERTIES DEPENDS itkKdTreeTest2 )
add_test(itkKdTreeTest3Plot ${DOXYGEN_DOT_EXECUTABLE} -Tpng -o  ${TEMP}/itkKdTreeTest3.png ${TEMP}/itkKdTreeTest3.dot)
set_tests_properties( itkKdTreeTest3Plot PROPERTIES DEPENDS itkKdTreeTest3 )
endif(DOXYGEN_DOT_EXECUTABLE)
add_test(itkKdTreeTest4 ${TestDriver} itkKdTreeTest1 1000 1000 1 )
add_test(itkKdTreeTest5 ${TestDriver} itkKdTreeTest1 1000 1000 2 )
add_test(itkKdTreeTest6 ${TestDriver} itkKdTreeTest1 1000 1000 3 )
add_test(itkKdTreeTest7 ${TestDriver} itkKdTreeTest1 1000 1000 5 )
add_test(itkKdTreeTest8 ${TestDriver} itkKdTreeTest1 1000 1000 10 )
add_test(itkKdTreeTest9 ${TestDriver} itkKdTreeTest1 1000 1000 20 )
add_test(itkKdTreeTest10 ${TestDriver} itkKdTreeTest1 1000 1000 100 )
add_test(itkWeightedCentroidKdTreeGeneratorTest1 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 1 )
add_test(itkWeightedCentroidKdTreeGeneratorTest2 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 2 )
add_test(itkWeightedCentroidKdTreeGeneratorTest3 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 3 )
add_test(itkWeightedCentroidKdTreeGeneratorTest4 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 5 )
add_test(itkWeightedCentroidKdTreeGeneratorTest5 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 10 )
add_test(itkWeightedCentroidKdTreeGeneratorTest6 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 20 )
add_test(itkWeightedCentroidKdTreeGeneratorTest7 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest1 1000 1000 100 )
add_test(itkWeightedCentroidKdTreeGeneratorTest8 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest8 1000 1000 1 )
add_test(itkWeightedCentroidKdTreeGeneratorTest9 ${TestDriver}
  itkWeightedCentroidKdTreeGeneratorTest9 1000 1000 1 )

add_test(itkGaussianMixtureModelComponentTest
         ${TestDriver}
         itkGaussianMixtureModelComponentTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

add_test(itkExpectationMaximizationMixtureModelEstimatorTest
         ${TestDriver}
         itkExpectationMaximizationMixtureModelEstimatorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)

add_test(itkImageClassifierFilterTest
         ${TestDriver}
        --compare ${ITK_DATA_ROOT}/Baseline/Statistics/ImageClassifierFilterTestClassifiedImageTest.png
         ${TEMP}/ImageClassifierFilterTestClassifiedImage.png
         itkImageClassifierFilterTest
         ${TEMP}/ImageClassifierFilterTestClassifiedImage.png)

add_test(itkChiSquareDistributionTest ${TestDriver}
  itkChiSquareDistributionTest
  )
add_test(itkGaussianDistributionTest ${TestDriver}
  itkGaussianDistributionTest
  )
add_test(itkTDistributionTest ${TestDriver} itkTDistributionTest)

endif( NOT ITK_DISABLE_CXX_TESTING )
