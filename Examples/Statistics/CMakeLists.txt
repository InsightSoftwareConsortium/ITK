project(StatisticsExamples)
include_regular_expression("^.*$")


add_executable(BayesianPluginClassifier BayesianPluginClassifier.cxx )
target_link_libraries(BayesianPluginClassifier ${ITK_LIBRARIES})

add_executable( BayesianClassifierInitializer BayesianClassifierInitializer.cxx )
target_link_libraries(BayesianClassifierInitializer ${ITK_LIBRARIES})

add_executable( BayesianClassifier BayesianClassifier.cxx )
target_link_libraries(BayesianClassifier ${ITK_LIBRARIES})

add_executable(EuclideanDistanceMetric EuclideanDistanceMetric.cxx )
target_link_libraries(EuclideanDistanceMetric ${ITK_LIBRARIES})

add_executable(ExpectationMaximizationMixtureModelEstimator ExpectationMaximizationMixtureModelEstimator.cxx )
target_link_libraries(ExpectationMaximizationMixtureModelEstimator ${ITK_LIBRARIES})

add_executable(GaussianMembershipFunction GaussianMembershipFunction.cxx )
target_link_libraries(GaussianMembershipFunction ${ITK_LIBRARIES})

add_executable(Histogram Histogram.cxx )
target_link_libraries(Histogram ${ITK_LIBRARIES})

add_executable(KdTree KdTree.cxx )
target_link_libraries(KdTree ${ITK_LIBRARIES})

add_executable(KdTreeBasedKMeansClustering KdTreeBasedKMeansClustering.cxx )
target_link_libraries(KdTreeBasedKMeansClustering ${ITK_LIBRARIES})

add_executable(ListSample ListSample.cxx )
target_link_libraries(ListSample ${ITK_LIBRARIES})

add_executable(SampleToHistogramFilter SampleToHistogramFilter.cxx )
target_link_libraries(SampleToHistogramFilter ${ITK_LIBRARIES})

add_executable(MaximumDecisionRule2 MaximumDecisionRule2.cxx )
target_link_libraries(MaximumDecisionRule2 ${ITK_LIBRARIES})

add_executable(MaximumRatioDecisionRule2 MaximumRatioDecisionRule2.cxx )
target_link_libraries(MaximumRatioDecisionRule2 ${ITK_LIBRARIES})

add_executable(MembershipSample MembershipSample.cxx )
target_link_libraries(MembershipSample ${ITK_LIBRARIES})

add_executable(MinimumDecisionRule2 MinimumDecisionRule2.cxx )
target_link_libraries(MinimumDecisionRule2 ${ITK_LIBRARIES})
#
# At the moment, itkNeighborhoodSampler is incomplete, so this example
# can't be updated
#
#add_executable(NeighborhoodSampler NeighborhoodSampler.cxx )
#target_link_libraries(NeighborhoodSampler ${ITK_LIBRARIES})

add_executable(NormalVariateGenerator NormalVariateGenerator.cxx )
target_link_libraries(NormalVariateGenerator ${ITK_LIBRARIES})

add_executable(PointSetToListSampleAdaptor PointSetToListSampleAdaptor.cxx )
target_link_libraries(PointSetToListSampleAdaptor ${ITK_LIBRARIES})

add_executable(SampleStatistics SampleStatistics.cxx )
target_link_libraries(SampleStatistics ${ITK_LIBRARIES})

add_executable(SampleSorting SampleSorting.cxx )
target_link_libraries(SampleSorting ${ITK_LIBRARIES})

add_executable(WeightedSampleStatistics WeightedSampleStatistics.cxx )
target_link_libraries(WeightedSampleStatistics ${ITK_LIBRARIES})

add_executable(ImageHistogram3 ImageHistogram3.cxx )
target_link_libraries(ImageHistogram3 ${ITK_LIBRARIES})

add_executable(ScalarImageMarkovRandomField1 ScalarImageMarkovRandomField1.cxx )
target_link_libraries(ScalarImageMarkovRandomField1 ${ITK_LIBRARIES})

add_executable(ScalarImageKmeansClassifier ScalarImageKmeansClassifier.cxx )
target_link_libraries(ScalarImageKmeansClassifier ${ITK_LIBRARIES})

add_executable(ImageToListSampleAdaptor ImageToListSampleAdaptor.cxx )
target_link_libraries(ImageToListSampleAdaptor ${ITK_LIBRARIES})

add_executable(ImageHistogram1 ImageHistogram1.cxx )
target_link_libraries(ImageHistogram1 ${ITK_LIBRARIES})

add_executable(ImageHistogram2 ImageHistogram2.cxx )
target_link_libraries(ImageHistogram2 ${ITK_LIBRARIES})

add_executable(ImageHistogram4 ImageHistogram4.cxx )
target_link_libraries(ImageHistogram4 ${ITK_LIBRARIES})

add_executable(ImageEntropy1 ImageEntropy1.cxx )
target_link_libraries(ImageEntropy1 ${ITK_LIBRARIES})

add_executable(ImageMutualInformation1 ImageMutualInformation1.cxx )
target_link_libraries(ImageMutualInformation1 ${ITK_LIBRARIES})

add_executable(ScalarImageKmeansModelEstimator ScalarImageKmeansModelEstimator.cxx )
target_link_libraries(ScalarImageKmeansModelEstimator ${ITK_LIBRARIES})

# Add some tests now, based on the examples.

if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Statistics)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

itk_add_test(NAME  ScalarImageKmeansClassifierTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ScalarImageKmeansClassifierOutput.png
            ${TEMP}/ScalarImageKmeansClassifierOutput.png
  $<TARGET_FILE:ScalarImageKmeansClassifier>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${TEMP}/ScalarImageKmeansClassifierOutput.png
        1 3 14.8 91.6 134.9
)

itk_add_test(NAME  ScalarImageMarkovRandomField1Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ScalarImageMarkovRandomField1Output.png
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
  $<TARGET_FILE:ScalarImageMarkovRandomField1>
            ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
            ${ITK_DATA_ROOT}/Input/Statistics/BrainT1Slice_AfterKMeansClassification.png
            ${TEMP}/ScalarImageMarkovRandomField1Output.png
            50 3 3 14.8 91.6 134.9
)

itk_add_test(NAME  BayesianPluginClassifierTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:BayesianPluginClassifier>)

itk_add_test(NAME  BayesianClassifierInitializerTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BayesianClassifierInitializerClass2Output.png
            ${TEMP}/BayesianClassifierInitializerClass2Output.png
  $<TARGET_FILE:BayesianClassifierInitializer>
            ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
            ${TEMP}/BayesianClassifierInitializerMemberships.mhd
            4 2 ${TEMP}/BayesianClassifierInitializerClass2Output.png
)

# This test uses inputs from BayesianClassifierInitializer.
# If that output changes, a new file should be placed in Examples/Data.
itk_add_test(NAME  BayesianClassifierTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BayesianClassifierLabelMap.png
            ${TEMP}/BayesianClassifierLabelMap.png
  $<TARGET_FILE:BayesianClassifier>
            ${ITK_SOURCE_DIR}/Examples/Data/BayesianClassifierInitializerMemberships.mhd
            ${TEMP}/BayesianClassifierLabelMap.png 2
)

itk_add_test(NAME  EuclideanDistanceTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:EuclideanDistanceMetric>)

itk_add_test(NAME  GaussianDensityFunctionTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:GaussianMembershipFunction>)

itk_add_test(NAME  MinimumDecisionRuleTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:MinimumDecisionRule2>)

itk_add_test(NAME  MaximumDecisionRuleTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:MaximumDecisionRule2>)

itk_add_test(NAME  MaximumRatioDecisionRuleTest COMMAND ${ITK_TEST_DRIVER}
          $<TARGET_FILE:MaximumRatioDecisionRule2>)


endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )
