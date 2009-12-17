IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Statistics)
SET(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

SET(STATISTICS_TESTS ${CXX_TEST_PATH}/itkStatisticsTests)
SET(STATISTICS_HEADER_TESTS ${CXX_TEST_PATH}/itkStatisticsHeaderTest)

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
ADD_TEST(itkImageToListGeneratorTest ${STATISTICS_TESTS} itkImageToListGeneratorTest)
ADD_TEST(itkImageToCooccurrenceListAdaptorTest ${STATISTICS_TESTS} 
         itkImageToCooccurrenceListAdaptorTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd)
ADD_TEST(itkImageToHistogramGeneratorTest ${STATISTICS_TESTS} itkImageToHistogramGeneratorTest)
ADD_TEST(itkKdTreeBasedKmeansEstimatorTest ${STATISTICS_TESTS} 
         itkKdTreeBasedKmeansEstimatorTest 
         ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat 
         1 28.54746 0.07)
ADD_TEST(itkKdTreeGeneratorTest ${STATISTICS_TESTS} 
         itkKdTreeGeneratorTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkKdTreeTest1 ${STATISTICS_TESTS} itkKdTreeTest1 100 100 1 ${TEMP}/itkKdTreeTest1_100.dot)
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
ADD_TEST(itkNthElementTest1a ${STATISTICS_TESTS} itkNthElementTest1)
ADD_TEST(itkNthElementTest1b ${STATISTICS_TESTS} itkNthElementTest1 ${ITK_DATA_ROOT}/Input/pointsForQuickSelect1.txt)
ADD_TEST(itkNthElementTest1c ${STATISTICS_TESTS} itkNthElementTest1 ${ITK_DATA_ROOT}/Input/pointsForQuickSelect2.txt)
ADD_TEST(itkNthElementTest1d ${STATISTICS_TESTS} itkNthElementTest1 ${ITK_DATA_ROOT}/Input/pointsForQuickSelect3.txt)
ADD_TEST(itkQuickSelectTest1a ${STATISTICS_TESTS} itkQuickSelectTest1)
ADD_TEST(itkQuickSelectTest1b ${STATISTICS_TESTS} itkQuickSelectTest1 ${ITK_DATA_ROOT}/Input/pointsForQuickSelect1.txt)
ADD_TEST(itkQuickSelectTest1c ${STATISTICS_TESTS} itkQuickSelectTest1 ${ITK_DATA_ROOT}/Input/pointsForQuickSelect2.txt)
ADD_TEST(itkQuickSelectTest1d ${STATISTICS_TESTS} itkQuickSelectTest1 ${ITK_DATA_ROOT}/Input/pointsForQuickSelect3.txt)
ADD_TEST(itkSampleClassifierTest ${STATISTICS_TESTS} 
         itkSampleClassifierTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkSampleClassifierWithMaskTest ${STATISTICS_TESTS} 
         itkSampleClassifierWithMaskTest ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
ADD_TEST(itkSampleMeanShiftClusteringFilterTest ${STATISTICS_TESTS}                itkSampleMeanShiftClusteringFilterTest ${ITK_DATA_ROOT}/Input/sf4.png)
IF( ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY )
ADD_TEST(itkSampleSelectiveMeanShiftBlurringFilterTest ${STATISTICS_TESTS}
  --compare ${BASELINE}/itkSampleSelectiveMeanShiftBlurringFilterTestPixelCentered.png
            ${TEMP}/itkSampleSelectiveMeanShiftBlurringFilterTestPixelCentered.png
            itkSampleSelectiveMeanShiftBlurringFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${TEMP}/itkSampleSelectiveMeanShiftBlurringFilterTestPixelCentered.png)
ELSE( ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY )
ADD_TEST(itkSampleSelectiveMeanShiftBlurringFilterTest ${STATISTICS_TESTS}
  --compare ${BASELINE}/itkSampleSelectiveMeanShiftBlurringFilterTest.png
            ${TEMP}/itkSampleSelectiveMeanShiftBlurringFilterTest.png
            itkSampleSelectiveMeanShiftBlurringFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${TEMP}/itkSampleSelectiveMeanShiftBlurringFilterTest.png)
ENDIF( ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY )
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

ADD_TEST(itkChiSquareDistributionTest ${STATISTICS_TESTS}
  itkChiSquareDistributionTest
  )
ADD_TEST(itkGaussianDistributionTest ${STATISTICS_TESTS}
  itkGaussianDistributionTest
  )
ADD_TEST(itkTDistributionTest ${STATISTICS_TESTS} itkTDistributionTest)

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
itkImageToListGeneratorTest.cxx
itkKdTreeBasedKmeansEstimatorTest.cxx
itkKdTreeGeneratorTest.cxx
itkKdTreeTest1.cxx
itkKdTreeTest2.cxx
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
itkNthElementTest1.cxx
itkQuickSelectTest1.cxx
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
itkWeightedCentroidKdTreeGeneratorTest1.cxx
itkGaussianDistributionTest.cxx
itkChiSquareDistributionTest.cxx
itkTDistributionTest.cxx
)

ADD_EXECUTABLE(itkStatisticsTests itkStatisticsTests.cxx ${Statistics_SRCS})
TARGET_LINK_LIBRARIES(itkStatisticsTests ITKIO ITKStatistics ITKNumerics)

ADD_EXECUTABLE(itkStatisticsHeaderTest itkStatisticsHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkStatisticsHeaderTest ITKIO ITKStatistics ITKNumerics)

ADD_TEST(itkStatisticsHeaderTest ${STATISTICS_HEADER_TESTS})

IF(ITK_OBJCXX_COMPILER_WORKS)
  SET(STATISTICS_OBJCXX_TEST ${CXX_TEST_PATH}/itkStatisticsHeaderObjCxxTest)
  ADD_TEST(itkStatisticsHeaderObjCxxTest ${STATISTICS_OBJCXX_TEST})
  ADD_EXECUTABLE(itkStatisticsHeaderObjCxxTest itkStatisticsHeaderObjCxxTest.mm)
  TARGET_LINK_LIBRARIES(itkStatisticsHeaderObjCxxTest ITKIO ITKStatistics ITKNumerics)
ENDIF(ITK_OBJCXX_COMPILER_WORKS)



ENDIF( NOT ITK_DISABLE_CXX_TESTING )


