PROJECT(StatisticsExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(BayesianPluginClassifier BayesianPluginClassifier.cxx )
TARGET_LINK_LIBRARIES(BayesianPluginClassifier ITKCommon ITKStatistics)

ADD_EXECUTABLE(EuclideanDistance EuclideanDistance.cxx )
TARGET_LINK_LIBRARIES(EuclideanDistance ITKCommon)

ADD_EXECUTABLE(ExpectationMaximizationMixtureModelEstimator ExpectationMaximizationMixtureModelEstimator.cxx )
TARGET_LINK_LIBRARIES(ExpectationMaximizationMixtureModelEstimator
ITKCommon ITKStatistics)

ADD_EXECUTABLE(GaussianDensityFunction GaussianDensityFunction.cxx )
TARGET_LINK_LIBRARIES(GaussianDensityFunction ITKCommon)

ADD_EXECUTABLE(Histogram Histogram.cxx )
TARGET_LINK_LIBRARIES(Histogram ITKCommon)

ADD_EXECUTABLE(ImageToListAdaptor ImageToListAdaptor.cxx )
TARGET_LINK_LIBRARIES(ImageToListAdaptor ITKCommon)

ADD_EXECUTABLE(KdTree KdTree.cxx )
TARGET_LINK_LIBRARIES(KdTree ITKCommon)

ADD_EXECUTABLE(KdTreeBasedKMeansClustering KdTreeBasedKMeansClustering.cxx )
TARGET_LINK_LIBRARIES(KdTreeBasedKMeansClustering ITKCommon ITKStatistics)

ADD_EXECUTABLE(ListSample ListSample.cxx )
TARGET_LINK_LIBRARIES(ListSample ITKCommon)

ADD_EXECUTABLE(ListSampleToHistogramFilter ListSampleToHistogramFilter.cxx )
TARGET_LINK_LIBRARIES(ListSampleToHistogramFilter ITKCommon)

ADD_EXECUTABLE(ListSampleToHistogramGenerator ListSampleToHistogramGenerator.cxx )
TARGET_LINK_LIBRARIES(ListSampleToHistogramGenerator ITKCommon)

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
TARGET_LINK_LIBRARIES(NormalVariateGenerator ITKCommon ITKStatistics)

ADD_EXECUTABLE(PointSetToListAdaptor PointSetToListAdaptor.cxx )
TARGET_LINK_LIBRARIES(PointSetToListAdaptor ITKCommon)

ADD_EXECUTABLE(NormalVariateGenerator NormalVariateGenerator.cxx )
TARGET_LINK_LIBRARIES(NormalVariateGenerator ITKCommon)

ADD_EXECUTABLE(SampleStatistics SampleStatistics.cxx )
TARGET_LINK_LIBRARIES(SampleStatistics ITKCommon)

ADD_EXECUTABLE(SampleToHistogramProjectionFilter SampleToHistogramProjectionFilter.cxx )
TARGET_LINK_LIBRARIES(SampleToHistogramProjectionFilter ITKCommon)

ADD_EXECUTABLE(SampleSorting SampleSorting.cxx )
TARGET_LINK_LIBRARIES(SampleSorting ITKCommon)

ADD_EXECUTABLE(WeightedSampleStatistics WeightedSampleStatistics.cxx )
TARGET_LINK_LIBRARIES(WeightedSampleStatistics ITKCommon)

ADD_EXECUTABLE(ImageHistogram1 ImageHistogram1.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram1 ITKIO ITKCommon )

ADD_EXECUTABLE(ImageHistogram2 ImageHistogram2.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram2 ITKIO ITKCommon )

ADD_EXECUTABLE(ImageHistogram3 ImageHistogram3.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram3 ITKIO ITKCommon )

ADD_EXECUTABLE(ImageHistogram4 ImageHistogram4.cxx )
TARGET_LINK_LIBRARIES(ImageHistogram4 ITKIO ITKCommon )

ADD_EXECUTABLE(ImageEntropy1 ImageEntropy1.cxx )
TARGET_LINK_LIBRARIES(ImageEntropy1 ITKIO ITKCommon )

ADD_EXECUTABLE(ImageMutualInformation1 ImageMutualInformation1.cxx )
TARGET_LINK_LIBRARIES(ImageMutualInformation1 ITKIO ITKCommon )

ADD_EXECUTABLE(ScalarImageKmeansModelEstimator ScalarImageKmeansModelEstimator.cxx )
TARGET_LINK_LIBRARIES(ScalarImageKmeansModelEstimator ITKIO ITKCommon )

ADD_EXECUTABLE(ScalarImageKmeansClassifier ScalarImageKmeansClassifier.cxx )
TARGET_LINK_LIBRARIES(ScalarImageKmeansClassifier ITKIO ITKCommon )

ADD_EXECUTABLE(ScalarImageMarkovRandomField1 ScalarImageMarkovRandomField1.cxx )
TARGET_LINK_LIBRARIES(ScalarImageMarkovRandomField1 ITKIO ITKCommon )

# Add some tests now, based on the examples.

IF( NOT ITK_DISABLE_CXX_TESTING )

SET(STATISTICS_EXAMPLES_WITH_TESTS ${CXX_TEST_PATH}/StatisticsExamplesTests)
SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Statistics)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST( ScalarImageKmeansClassifierTest ${STATISTICS_EXAMPLES_WITH_TESTS}
  --compare ${BASELINE}/ScalarImageKmeansClassifierOutput.png
            ${TEMP}/ScalarImageKmeansClassifierOutput.png
  ScalarImageKmeansClassifierTest 
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${TEMP}/ScalarImageKmeansClassifierOutput.png
        1 3 14.8 91.6 134.9
)

ADD_TEST( ScalarImageMarkovRandomField1Test ${STATISTICS_EXAMPLES_WITH_TESTS}
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

ADD_TEST( EuclideanDistanceTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          EuclideanDistanceTest)

ADD_TEST( GaussianDensityFunctionTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          GaussianDensityFunctionTest)

ADD_TEST( MinimumDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          MinimumDecisionRuleTest)

ADD_TEST( MaximumDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          MaximumDecisionRuleTest)

ADD_TEST( MaximumRatioDecisionRuleTest ${STATISTICS_EXAMPLES_WITH_TESTS}
          MaximumRatioDecisionRuleTest)

ADD_EXECUTABLE( StatisticsExamplesTests StatisticsExamplesTests.cxx)
TARGET_LINK_LIBRARIES( StatisticsExamplesTests ITKIO ITKCommon ITKStatistics)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )
