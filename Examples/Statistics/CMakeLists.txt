PROJECT(StatisticsExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(BayesianPluginClassifier BayesianPluginClassifier.cxx )
TARGET_LINK_LIBRARIES(BayesianPluginClassifier ITKStatistics)

ADD_EXECUTABLE( BayesianClassifierInitializer BayesianClassifierInitializer.cxx )
TARGET_LINK_LIBRARIES(BayesianClassifierInitializer ITKStatistics ITKIO)

ADD_EXECUTABLE( BayesianClassifier BayesianClassifier.cxx )
TARGET_LINK_LIBRARIES(BayesianClassifier ITKStatistics ITKIO)

ADD_EXECUTABLE(EuclideanDistance EuclideanDistance.cxx )
TARGET_LINK_LIBRARIES(EuclideanDistance ITKCommon)

ADD_EXECUTABLE(ExpectationMaximizationMixtureModelEstimator ExpectationMaximizationMixtureModelEstimator.cxx )
TARGET_LINK_LIBRARIES(ExpectationMaximizationMixtureModelEstimator
ITKStatistics)

ADD_EXECUTABLE(GaussianDensityFunction GaussianDensityFunction.cxx )
TARGET_LINK_LIBRARIES(GaussianDensityFunction ITKCommon)

ADD_EXECUTABLE(Histogram Histogram.cxx )
TARGET_LINK_LIBRARIES(Histogram ITKStatistics)

ADD_EXECUTABLE(KdTree KdTree.cxx )
TARGET_LINK_LIBRARIES(KdTree ITKCommon)

ADD_EXECUTABLE(KdTreeBasedKMeansClustering KdTreeBasedKMeansClustering.cxx )
TARGET_LINK_LIBRARIES(KdTreeBasedKMeansClustering ITKStatistics)

ADD_EXECUTABLE(ListSample ListSample.cxx )
TARGET_LINK_LIBRARIES(ListSample ITKCommon)

ADD_EXECUTABLE(ListSampleToHistogramFilter ListSampleToHistogramFilter.cxx )
TARGET_LINK_LIBRARIES(ListSampleToHistogramFilter ITKStatistics)

ADD_EXECUTABLE(ListSampleToHistogramGenerator ListSampleToHistogramGenerator.cxx )
TARGET_LINK_LIBRARIES(ListSampleToHistogramGenerator ITKStatistics)

ADD_EXECUTABLE(MaximumDecisionRule MaximumDecisionRule.cxx )
TARGET_LINK_LIBRARIES(MaximumDecisionRule ITKCommon)

ADD_EXECUTABLE(MaximumRatioDecisionRule MaximumRatioDecisionRule.cxx )
TARGET_LINK_LIBRARIES(MaximumRatioDecisionRule ITKCommon)

ADD_EXECUTABLE(MembershipSample MembershipSample.cxx )
TARGET_LINK_LIBRARIES(MembershipSample ITKCommon)

ADD_EXECUTABLE(MinimumDecisionRule MinimumDecisionRule.cxx )
TARGET_LINK_LIBRARIES(MinimumDecisionRule ITKCommon)

ADD_EXECUTABLE(NeighborhoodSampler NeighborhoodSampler.cxx )
TARGET_LINK_LIBRARIES(NeighborhoodSampler ITKCommon)

ADD_EXECUTABLE(NormalVariateGenerator NormalVariateGenerator.cxx )
TARGET_LINK_LIBRARIES(NormalVariateGenerator ITKStatistics)

ADD_EXECUTABLE(PointSetToListAdaptor PointSetToListAdaptor.cxx )
TARGET_LINK_LIBRARIES(PointSetToListAdaptor ITKCommon)


ADD_EXECUTABLE(SampleStatistics SampleStatistics.cxx )
TARGET_LINK_LIBRARIES(SampleStatistics ITKCommon)

ADD_EXECUTABLE(SampleToHistogramProjectionFilter SampleToHistogramProjectionFilter.cxx )
TARGET_LINK_LIBRARIES(SampleToHistogramProjectionFilter ITKStatistics)

ADD_EXECUTABLE(SampleSorting SampleSorting.cxx )
TARGET_LINK_LIBRARIES(SampleSorting ITKCommon)

ADD_EXECUTABLE(WeightedSampleStatistics WeightedSampleStatistics.cxx )
TARGET_LINK_LIBRARIES(WeightedSampleStatistics ITKCommon)

IF(NOT BORLAND)

ADD_EXECUTABLE(ImageHistogram3 ImageHistogram3.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram3 ITKIO ITKStatistics )

ADD_EXECUTABLE(ScalarImageMarkovRandomField1 ScalarImageMarkovRandomField1.cxx )
TARGET_LINK_LIBRARIES(ScalarImageMarkovRandomField1 ITKIO )

ADD_EXECUTABLE(ScalarImageKmeansClassifier ScalarImageKmeansClassifier.cxx )
TARGET_LINK_LIBRARIES(ScalarImageKmeansClassifier ITKIO )

ADD_EXECUTABLE(ImageToListAdaptor ImageToListAdaptor.cxx )
TARGET_LINK_LIBRARIES(ImageToListAdaptor ITKCommon)

ADD_EXECUTABLE(ImageHistogram1 ImageHistogram1.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram1 ITKIO ITKStatistics )

ADD_EXECUTABLE(ImageHistogram2 ImageHistogram2.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram2 ITKIO ITKStatistics )

ADD_EXECUTABLE(ImageHistogram4 ImageHistogram4.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram4 ITKIO ITKStatistics )

ADD_EXECUTABLE(ImageEntropy1 ImageEntropy1.cxx )
TARGET_LINK_LIBRARIES(ImageEntropy1 ITKIO ITKStatistics)

ADD_EXECUTABLE(ImageMutualInformation1 ImageMutualInformation1.cxx )
TARGET_LINK_LIBRARIES(ImageMutualInformation1 ITKIO ITKStatistics )

ADD_EXECUTABLE(ScalarImageKmeansModelEstimator ScalarImageKmeansModelEstimator.cxx )
TARGET_LINK_LIBRARIES(ScalarImageKmeansModelEstimator ITKIO )

ENDIF(NOT BORLAND)


# Add some tests now, based on the examples.

IF( NOT ITK_DISABLE_CXX_TESTING )

IF(BUILD_TESTING)

IF(NOT BORLAND)

SET(STATISTICS_EXAMPLES_WITH_TESTS ${CXX_TEST_PATH}/StatisticsExamplesTests)
SET(STATISTICS_EXAMPLES_WITH_TESTS2 ${CXX_TEST_PATH}/StatisticsExamplesTests2)
SET(STATISTICS_EXAMPLES_WITH_TESTS3 ${CXX_TEST_PATH}/StatisticsExamplesTests3)
SET(STATISTICS_EXAMPLES_WITH_TESTS4 ${CXX_TEST_PATH}/StatisticsExamplesTests4)
SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Statistics)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST( ScalarImageKmeansClassifierTest ${STATISTICS_EXAMPLES_WITH_TESTS3}
  --compare ${BASELINE}/ScalarImageKmeansClassifierOutput.png
            ${TEMP}/ScalarImageKmeansClassifierOutput.png
  ScalarImageKmeansClassifierTest 
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${TEMP}/ScalarImageKmeansClassifierOutput.png
        1 3 14.8 91.6 134.9
)

ADD_TEST( ScalarImageMarkovRandomField1Test ${STATISTICS_EXAMPLES_WITH_TESTS3}
  --compare ${BASELINE}/ScalarImageMarkovRandomField1Output.png
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
  ScalarImageMarkovRandomField1Test
            ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
            ${ITK_DATA_ROOT}/Input/Statistics/BrainT1Slice_AfterKMeansClassification.png
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
            50 3 3 14.8 91.6 134.9
)

ADD_TEST( BayesianPluginClassifierTest ${STATISTICS_EXAMPLES_WITH_TESTS} 
          BayesianPluginClassifierTest)
        
ADD_TEST( BayesianClassifierInitializerTest ${STATISTICS_EXAMPLES_WITH_TESTS4}
  --compare ${BASELINE}/BayesianClassifierInitializerClass2Output.png 
            ${TEMP}/BayesianClassifierInitializerClass2Output.png
  BayesianClassifierInitializerTest
            ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
            ${TEMP}/BayesianClassifierInitializerMemberships.mhd
            4 2 ${TEMP}/BayesianClassifierInitializerClass2Output.png
)

# This test uses inputs from BayesianClassifierInitializer.
# If that output changes, a new file should be placed in Examples/Data.
ADD_TEST( BayesianClassifierTest ${STATISTICS_EXAMPLES_WITH_TESTS4}
  --compare ${BASELINE}/BayesianClassifierLabelMap.png 
            ${TEMP}/BayesianClassifierLabelMap.png
  BayesianClassifierTest
            ${ITK_SOURCE_DIR}/Examples/Data/BayesianClassifierInitializerMemberships.mhd
            ${TEMP}/BayesianClassifierLabelMap.png 2
)

ADD_TEST( EuclideanDistanceTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          EuclideanDistanceTest)

ADD_TEST( GaussianDensityFunctionTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          GaussianDensityFunctionTest)

ADD_TEST( MinimumDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS2}
          MinimumDecisionRuleTest)

ADD_TEST( MaximumDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS2}
          MaximumDecisionRuleTest)

ADD_TEST( MaximumRatioDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS2}
          MaximumRatioDecisionRuleTest)

ADD_EXECUTABLE( StatisticsExamplesTests StatisticsExamplesTests.cxx)
TARGET_LINK_LIBRARIES( StatisticsExamplesTests ITKIO ITKStatistics)

ADD_EXECUTABLE( StatisticsExamplesTests2 StatisticsExamplesTests2.cxx)
TARGET_LINK_LIBRARIES( StatisticsExamplesTests2 ITKIO ITKStatistics)

ADD_EXECUTABLE( StatisticsExamplesTests4 StatisticsExamplesTests4.cxx)
TARGET_LINK_LIBRARIES( StatisticsExamplesTests4 ITKIO ITKStatistics)

ADD_EXECUTABLE( StatisticsExamplesTests3 StatisticsExamplesTests3.cxx)
TARGET_LINK_LIBRARIES( StatisticsExamplesTests3 ITKIO ITKStatistics)

ENDIF(NOT BORLAND)

ENDIF(BUILD_TESTING)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )
