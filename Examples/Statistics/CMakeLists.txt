project(StatisticsExamples)

add_executable(BayesianPluginClassifier BayesianPluginClassifier.cxx)
target_link_libraries(
  BayesianPluginClassifier
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(BayesianClassifierInitializer BayesianClassifierInitializer.cxx)
target_link_libraries(
  BayesianClassifierInitializer
  PRIVATE
    ITKImageIO
    ITKClassifiersModule
    ITKCommonModule
)

add_executable(BayesianClassifier BayesianClassifier.cxx)
target_link_libraries(
  BayesianClassifier
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKClassifiersModule
)

add_executable(EuclideanDistanceMetric EuclideanDistanceMetric.cxx)
target_link_libraries(
  EuclideanDistanceMetric
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(
  ExpectationMaximizationMixtureModelEstimator
  ExpectationMaximizationMixtureModelEstimator.cxx
)
target_link_libraries(
  ExpectationMaximizationMixtureModelEstimator
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(GaussianMembershipFunction GaussianMembershipFunction.cxx)
target_link_libraries(
  GaussianMembershipFunction
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(Histogram Histogram.cxx)
target_link_libraries(Histogram PRIVATE ITKStatisticsModule)

add_executable(KdTree KdTree.cxx)
target_link_libraries(
  KdTree
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(KdTreeBasedKMeansClustering KdTreeBasedKMeansClustering.cxx)
target_link_libraries(
  KdTreeBasedKMeansClustering
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(ListSample ListSample.cxx)
target_link_libraries(
  ListSample
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(SampleToHistogramFilter SampleToHistogramFilter.cxx)
target_link_libraries(
  SampleToHistogramFilter
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(MaximumDecisionRule MaximumDecisionRule.cxx)
target_link_libraries(MaximumDecisionRule PRIVATE ITKStatisticsModule)

add_executable(MaximumRatioDecisionRule MaximumRatioDecisionRule.cxx)
target_link_libraries(MaximumRatioDecisionRule PRIVATE ITKStatisticsModule)

add_executable(MembershipSample MembershipSample.cxx)
target_link_libraries(
  MembershipSample
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(MinimumDecisionRule MinimumDecisionRule.cxx)
target_link_libraries(MinimumDecisionRule PRIVATE ITKStatisticsModule)
#
# At the moment, itkNeighborhoodSampler is incomplete, so this example
# can't be updated
#
#add_executable(NeighborhoodSampler NeighborhoodSampler.cxx)
#target_link_libraries(NeighborhoodSampler ${ITK_LIBRARIES})

add_executable(NormalVariateGenerator NormalVariateGenerator.cxx)
target_link_libraries(NormalVariateGenerator PRIVATE ITKStatisticsModule)

add_executable(PointSetToListSampleAdaptor PointSetToListSampleAdaptor.cxx)
target_link_libraries(
  PointSetToListSampleAdaptor
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(SampleStatistics SampleStatistics.cxx)
target_link_libraries(
  SampleStatistics
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(SampleSorting SampleSorting.cxx)
target_link_libraries(
  SampleSorting
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(WeightedSampleStatistics WeightedSampleStatistics.cxx)
target_link_libraries(
  WeightedSampleStatistics
  PRIVATE
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(ImageHistogram3 ImageHistogram3.cxx)
target_link_libraries(
  ImageHistogram3
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(ScalarImageMarkovRandomField1 ScalarImageMarkovRandomField1.cxx)
target_link_libraries(
  ScalarImageMarkovRandomField1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
    ITKImageComposeModule
    ITKMarkovRandomFieldsClassifiersModule
)

add_executable(ScalarImageKmeansClassifier ScalarImageKmeansClassifier.cxx)
target_link_libraries(
  ScalarImageKmeansClassifier
  PRIVATE
    ITKImageIO
    ITKClassifiersModule
    ITKCommonModule
)

add_executable(ImageToListSampleAdaptor ImageToListSampleAdaptor.cxx)
target_link_libraries(
  ImageToListSampleAdaptor
  PRIVATE
    ITKCommonModule
    ITKImageComposeModule
    ITKStatisticsModule
    ITKTestKernelModule
)

add_executable(ImageHistogram1 ImageHistogram1.cxx)
target_link_libraries(
  ImageHistogram1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(ImageHistogram2 ImageHistogram2.cxx)
target_link_libraries(
  ImageHistogram2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(ImageHistogram4 ImageHistogram4.cxx)
target_link_libraries(
  ImageHistogram4
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(ImageEntropy1 ImageEntropy1.cxx)
target_link_libraries(
  ImageEntropy1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKStatisticsModule
)

add_executable(ImageMutualInformation1 ImageMutualInformation1.cxx)
target_link_libraries(
  ImageMutualInformation1
  PRIVATE
    ITKImageIO
    ITKStatisticsModule
    ITKCommonModule
    ITKImageComposeModule
)

add_executable(
  ScalarImageKmeansModelEstimator
  ScalarImageKmeansModelEstimator.cxx
)
target_link_libraries(
  ScalarImageKmeansModelEstimator
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKStatisticsModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
