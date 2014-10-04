project(StatisticsExamples)

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

add_executable(MaximumDecisionRule MaximumDecisionRule.cxx )
target_link_libraries(MaximumDecisionRule ${ITK_LIBRARIES})

add_executable(MaximumRatioDecisionRule MaximumRatioDecisionRule.cxx )
target_link_libraries(MaximumRatioDecisionRule ${ITK_LIBRARIES})

add_executable(MembershipSample MembershipSample.cxx )
target_link_libraries(MembershipSample ${ITK_LIBRARIES})

add_executable(MinimumDecisionRule MinimumDecisionRule.cxx )
target_link_libraries(MinimumDecisionRule ${ITK_LIBRARIES})
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

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
