itk_module_test()
set(ITK-StatisticsTests
itkKalmanLinearEstimatorTest.cxx
itkStatisticsHeaderTest.cxx
itkMaximumDecisionRuleTest.cxx
itkMinimumDecisionRuleTest.cxx
itkMaximumRatioDecisionRuleTest.cxx
itkHistogramTest.cxx
itkHistogramToTextureFeaturesFilterTest.cxx
itkCovarianceSampleFilterTest.cxx
itkCovarianceSampleFilterTest2.cxx
itkCovarianceSampleFilterTest3.cxx
itkWeightedCovarianceSampleFilterTest.cxx
itkWeightedCovarianceSampleFilterTest2.cxx
itkWeightedMeanSampleFilterTest.cxx
itkWeightedCentroidKdTreeGeneratorTest1.cxx
itkWeightedCentroidKdTreeGeneratorTest8.cxx
itkWeightedCentroidKdTreeGeneratorTest9.cxx
itkMahalanobisDistanceMetricTest.cxx
itkManhattanDistanceMetricTest.cxx
itkDistanceMetricTest.cxx
itkDistanceMetricTest2.cxx
itkDistanceToCentroidMembershipFunctionTest.cxx
itkEuclideanDistanceMetricTest.cxx
itkEuclideanSquareDistanceMetricTest.cxx
)

CreateTestDriver(ITK-Statistics  "${ITK-Statistics-Test_LIBRARIES}" "${ITK-StatisticsTests}")

add_test(NAME itkStatisticsHeaderTest
      COMMAND ITK-StatisticsTestDriver itkStatisticsHeaderTest)
add_test(NAME itkKalmanLinearEstimatorTest
      COMMAND ITK-StatisticsTestDriver itkKalmanLinearEstimatorTest)
add_test(NAME itkMaximumDecisionRuleTest
      COMMAND ITK-StatisticsTestDriver itkMaximumDecisionRuleTest)
add_test(NAME itkMinimumDecisionRuleTest
      COMMAND ITK-StatisticsTestDriver itkMinimumDecisionRuleTest)
add_test(NAME itkMaximumRatioDecisionRuleTest
      COMMAND ITK-StatisticsTestDriver itkMaximumRatioDecisionRuleTest)
add_test(NAME itkHistogramTest
      COMMAND ITK-StatisticsTestDriver itkHistogramTest)
add_test(NAME itkHistogramToTextureFeaturesFilterTest
      COMMAND ITK-StatisticsTestDriver itkHistogramToTextureFeaturesFilterTest)
add_test(NAME itkCovarianceSampleFilterTest
      COMMAND ITK-StatisticsTestDriver itkCovarianceSampleFilterTest)
add_test(NAME itkCovarianceSampleFilterTest2
      COMMAND ITK-StatisticsTestDriver itkCovarianceSampleFilterTest2)
add_test(NAME itkCovarianceSampleFilterTest3
      COMMAND ITK-StatisticsTestDriver itkCovarianceSampleFilterTest3)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest1
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 1)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest2
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 2)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest3
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 3)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest4
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 5)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest5
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 10)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest6
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 20)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest7
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 100)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest8
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest8
              1000 1000 1)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest9
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest9
              1000 1000 1)
add_test(NAME itkWeightedCovarianceSampleFilterTest
      COMMAND ITK-StatisticsTestDriver itkWeightedCovarianceSampleFilterTest)
add_test(NAME itkWeightedCovarianceSampleFilterTest2
      COMMAND ITK-StatisticsTestDriver itkWeightedCovarianceSampleFilterTest2)
add_test(NAME itkWeightedMeanSampleFilterTest
      COMMAND ITK-StatisticsTestDriver itkWeightedMeanSampleFilterTest)
add_test(NAME itkDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkDistanceMetricTest)
add_test(NAME itkDistanceMetricTest2
      COMMAND ITK-StatisticsTestDriver itkDistanceMetricTest2)
add_test(NAME itkDistanceToCentroidMembershipFunctionTest
      COMMAND ITK-StatisticsTestDriver itkDistanceToCentroidMembershipFunctionTest)
add_test(NAME itkEuclideanDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkEuclideanDistanceMetricTest)
add_test(NAME itkEuclideanSquareDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkEuclideanSquareDistanceMetricTest)
add_test(NAME itkMahalanobisDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkMahalanobisDistanceMetricTest)
add_test(NAME itkManhattanDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkManhattanDistanceMetricTest)
