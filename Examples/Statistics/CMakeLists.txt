project(StatisticsExamples)
include_regular_expression("^.*$")

add_executable(BayesianPluginClassifier BayesianPluginClassifier.cxx )
target_link_libraries(BayesianPluginClassifier ITKStatistics)

add_executable( BayesianClassifierInitializer BayesianClassifierInitializer.cxx )
target_link_libraries(BayesianClassifierInitializer ITKStatistics ITKIO)

add_executable( BayesianClassifier BayesianClassifier.cxx )
target_link_libraries(BayesianClassifier ITKStatistics ITKIO)

add_executable(EuclideanDistance EuclideanDistance.cxx )
target_link_libraries(EuclideanDistance ITKCommon)

add_executable(ExpectationMaximizationMixtureModelEstimator ExpectationMaximizationMixtureModelEstimator.cxx )
target_link_libraries(ExpectationMaximizationMixtureModelEstimator
ITKStatistics)

add_executable(GaussianDensityFunction GaussianDensityFunction.cxx )
target_link_libraries(GaussianDensityFunction ITKCommon)

add_executable(Histogram Histogram.cxx )
target_link_libraries(Histogram ITKStatistics)

add_executable(KdTree KdTree.cxx )
target_link_libraries(KdTree ITKCommon)

add_executable(KdTreeBasedKMeansClustering KdTreeBasedKMeansClustering.cxx )
target_link_libraries(KdTreeBasedKMeansClustering ITKStatistics)

add_executable(ListSample ListSample.cxx )
target_link_libraries(ListSample ITKCommon)

add_executable(ListSampleToHistogramFilter ListSampleToHistogramFilter.cxx )
target_link_libraries(ListSampleToHistogramFilter ITKStatistics)

add_executable(ListSampleToHistogramGenerator ListSampleToHistogramGenerator.cxx )
target_link_libraries(ListSampleToHistogramGenerator ITKStatistics)

add_executable(MaximumDecisionRule MaximumDecisionRule.cxx )
target_link_libraries(MaximumDecisionRule ITKCommon)

add_executable(MaximumRatioDecisionRule MaximumRatioDecisionRule.cxx )
target_link_libraries(MaximumRatioDecisionRule ITKCommon)

add_executable(MembershipSample MembershipSample.cxx )
target_link_libraries(MembershipSample ITKCommon)

add_executable(MinimumDecisionRule MinimumDecisionRule.cxx )
target_link_libraries(MinimumDecisionRule ITKCommon)

add_executable(NeighborhoodSampler NeighborhoodSampler.cxx )
target_link_libraries(NeighborhoodSampler ITKCommon)

add_executable(NormalVariateGenerator NormalVariateGenerator.cxx )
target_link_libraries(NormalVariateGenerator ITKStatistics)

add_executable(PointSetToListAdaptor PointSetToListAdaptor.cxx )
target_link_libraries(PointSetToListAdaptor ITKCommon)


add_executable(SampleStatistics SampleStatistics.cxx )
target_link_libraries(SampleStatistics ITKCommon)

add_executable(SampleToHistogramProjectionFilter SampleToHistogramProjectionFilter.cxx )
target_link_libraries(SampleToHistogramProjectionFilter ITKStatistics)

add_executable(SampleSorting SampleSorting.cxx )
target_link_libraries(SampleSorting ITKCommon)

add_executable(WeightedSampleStatistics WeightedSampleStatistics.cxx )
target_link_libraries(WeightedSampleStatistics ITKCommon)

add_executable(ImageHistogram3 ImageHistogram3.cxx )
target_link_libraries(ImageHistogram3 ITKIO ITKStatistics )

add_executable(ScalarImageMarkovRandomField1 ScalarImageMarkovRandomField1.cxx )
target_link_libraries(ScalarImageMarkovRandomField1 ITKIO )

add_executable(ScalarImageKmeansClassifier ScalarImageKmeansClassifier.cxx )
target_link_libraries(ScalarImageKmeansClassifier ITKIO )

add_executable(ImageToListAdaptor ImageToListAdaptor.cxx )
target_link_libraries(ImageToListAdaptor ITKCommon)

add_executable(ImageHistogram1 ImageHistogram1.cxx )
target_link_libraries(ImageHistogram1 ITKIO ITKStatistics )

add_executable(ImageHistogram2 ImageHistogram2.cxx )
target_link_libraries(ImageHistogram2 ITKIO ITKStatistics )

add_executable(ImageHistogram4 ImageHistogram4.cxx )
target_link_libraries(ImageHistogram4 ITKIO ITKStatistics )

add_executable(ImageEntropy1 ImageEntropy1.cxx )
target_link_libraries(ImageEntropy1 ITKIO ITKStatistics)

add_executable(ImageMutualInformation1 ImageMutualInformation1.cxx )
target_link_libraries(ImageMutualInformation1 ITKIO ITKStatistics )

add_executable(ScalarImageKmeansModelEstimator ScalarImageKmeansModelEstimator.cxx )
target_link_libraries(ScalarImageKmeansModelEstimator ITKIO )

# Add some tests now, based on the examples.

if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

set(STATISTICS_EXAMPLES_WITH_TESTS ${CXX_TEST_PATH}/StatisticsExamplesTests)
set(STATISTICS_EXAMPLES_WITH_TESTS2 ${CXX_TEST_PATH}/StatisticsExamplesTests2)
set(STATISTICS_EXAMPLES_WITH_TESTS3 ${CXX_TEST_PATH}/StatisticsExamplesTests3)
set(STATISTICS_EXAMPLES_WITH_TESTS4 ${CXX_TEST_PATH}/StatisticsExamplesTests4)
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Statistics)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

add_test( ScalarImageKmeansClassifierTest ${STATISTICS_EXAMPLES_WITH_TESTS3}
  --compare ${BASELINE}/ScalarImageKmeansClassifierOutput.png
            ${TEMP}/ScalarImageKmeansClassifierOutput.png
  ScalarImageKmeansClassifierTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${TEMP}/ScalarImageKmeansClassifierOutput.png
        1 3 14.8 91.6 134.9
)

add_test( ScalarImageMarkovRandomField1Test ${STATISTICS_EXAMPLES_WITH_TESTS3}
  --compare ${BASELINE}/ScalarImageMarkovRandomField1Output.png
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
  ScalarImageMarkovRandomField1Test
            ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
            ${ITK_DATA_ROOT}/Input/Statistics/BrainT1Slice_AfterKMeansClassification.png
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
            50 3 3 14.8 91.6 134.9
)

add_test( BayesianPluginClassifierTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          BayesianPluginClassifierTest)

add_test( BayesianClassifierInitializerTest ${STATISTICS_EXAMPLES_WITH_TESTS4}
  --compare ${BASELINE}/BayesianClassifierInitializerClass2Output.png
            ${TEMP}/BayesianClassifierInitializerClass2Output.png
  BayesianClassifierInitializerTest
            ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
            ${TEMP}/BayesianClassifierInitializerMemberships.mhd
            4 2 ${TEMP}/BayesianClassifierInitializerClass2Output.png
)

# This test uses inputs from BayesianClassifierInitializer.
# If that output changes, a new file should be placed in Examples/Data.
add_test( BayesianClassifierTest ${STATISTICS_EXAMPLES_WITH_TESTS4}
  --compare ${BASELINE}/BayesianClassifierLabelMap.png
            ${TEMP}/BayesianClassifierLabelMap.png
  BayesianClassifierTest
            ${ITK_SOURCE_DIR}/Examples/Data/BayesianClassifierInitializerMemberships.mhd
            ${TEMP}/BayesianClassifierLabelMap.png 2
)

add_test( EuclideanDistanceTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          EuclideanDistanceTest)

add_test( GaussianDensityFunctionTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          GaussianDensityFunctionTest)

add_test( MinimumDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS2}
          MinimumDecisionRuleTest)

add_test( MaximumDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS2}
          MaximumDecisionRuleTest)

add_test( MaximumRatioDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS2}
          MaximumRatioDecisionRuleTest)

add_executable( StatisticsExamplesTests StatisticsExamplesTests.cxx)
target_link_libraries( StatisticsExamplesTests ITKIO ITKStatistics)

add_executable( StatisticsExamplesTests2 StatisticsExamplesTests2.cxx)
target_link_libraries( StatisticsExamplesTests2 ITKIO ITKStatistics)

add_executable( StatisticsExamplesTests4 StatisticsExamplesTests4.cxx)
target_link_libraries( StatisticsExamplesTests4 ITKIO ITKStatistics)

add_executable( StatisticsExamplesTests3 StatisticsExamplesTests3.cxx)
target_link_libraries( StatisticsExamplesTests3 ITKIO ITKStatistics)

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )
