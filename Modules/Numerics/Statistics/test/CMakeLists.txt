itk_module_test()
set(ITK-StatisticsTests
itkDecisionRuleTest.cxx
itkDenseFrequencyContainer2Test.cxx
itkExpectationMaximizationMixtureModelEstimatorTest.cxx
itkKalmanLinearEstimatorTest.cxx
itkStatisticsHeaderTest.cxx
itkMaximumDecisionRuleTest.cxx
itkMinimumDecisionRuleTest.cxx
itkMaximumRatioDecisionRuleTest.cxx
itkHistogramTest.cxx
itkHistogramToTextureFeaturesFilterTest.cxx
itkChiSquareDistributionTest.cxx
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

itk_add_test(NAME itkStatisticsHeaderTest
      COMMAND ITK-StatisticsTestDriver itkStatisticsHeaderTest)
itk_add_test(NAME itkDecisionRuleTest
      COMMAND ITK-StatisticsTestDriver itkDecisionRuleTest)
itk_add_test(NAME itkDenseFrequencyContainer2Test
      COMMAND ITK-StatisticsTestDriver itkDenseFrequencyContainer2Test)
itk_add_test(NAME itkExpectationMaximizationMixtureModelEstimatorTest
      COMMAND ITK-StatisticsTestDriver itkExpectationMaximizationMixtureModelEstimatorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
itk_add_test(NAME itkKalmanLinearEstimatorTest
      COMMAND ITK-StatisticsTestDriver itkKalmanLinearEstimatorTest)
itk_add_test(NAME itkMaximumDecisionRuleTest
      COMMAND ITK-StatisticsTestDriver itkMaximumDecisionRuleTest)
itk_add_test(NAME itkMinimumDecisionRuleTest
      COMMAND ITK-StatisticsTestDriver itkMinimumDecisionRuleTest)
itk_add_test(NAME itkMaximumRatioDecisionRuleTest
      COMMAND ITK-StatisticsTestDriver itkMaximumRatioDecisionRuleTest)
itk_add_test(NAME itkHistogramTest
      COMMAND ITK-StatisticsTestDriver itkHistogramTest)
itk_add_test(NAME itkHistogramToTextureFeaturesFilterTest
      COMMAND ITK-StatisticsTestDriver itkHistogramToTextureFeaturesFilterTest)
itk_add_test(NAME itkChiSquareDistributionTest
      COMMAND ITK-StatisticsTestDriver itkChiSquareDistributionTest)
itk_add_test(NAME itkCovarianceSampleFilterTest
      COMMAND ITK-StatisticsTestDriver itkCovarianceSampleFilterTest)
itk_add_test(NAME itkCovarianceSampleFilterTest2
      COMMAND ITK-StatisticsTestDriver itkCovarianceSampleFilterTest2)
itk_add_test(NAME itkCovarianceSampleFilterTest3
      COMMAND ITK-StatisticsTestDriver itkCovarianceSampleFilterTest3)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest1
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 1)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest2
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 2)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest3
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 3)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest4
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 5)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest5
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 10)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest6
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 20)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest7
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 100)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest8
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest8
              1000 1000 1)
itk_add_test(NAME itkWeightedCentroidKdTreeGeneratorTest9
      COMMAND ITK-StatisticsTestDriver itkWeightedCentroidKdTreeGeneratorTest9
              1000 1000 1)
itk_add_test(NAME itkWeightedCovarianceSampleFilterTest
      COMMAND ITK-StatisticsTestDriver itkWeightedCovarianceSampleFilterTest)
itk_add_test(NAME itkWeightedCovarianceSampleFilterTest2
      COMMAND ITK-StatisticsTestDriver itkWeightedCovarianceSampleFilterTest2)
itk_add_test(NAME itkWeightedMeanSampleFilterTest
      COMMAND ITK-StatisticsTestDriver itkWeightedMeanSampleFilterTest)
itk_add_test(NAME itkDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkDistanceMetricTest)
itk_add_test(NAME itkDistanceMetricTest2
      COMMAND ITK-StatisticsTestDriver itkDistanceMetricTest2)
itk_add_test(NAME itkDistanceToCentroidMembershipFunctionTest
      COMMAND ITK-StatisticsTestDriver itkDistanceToCentroidMembershipFunctionTest)
itk_add_test(NAME itkEuclideanDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkEuclideanDistanceMetricTest)
itk_add_test(NAME itkEuclideanSquareDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkEuclideanSquareDistanceMetricTest)
itk_add_test(NAME itkMahalanobisDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkMahalanobisDistanceMetricTest)
itk_add_test(NAME itkManhattanDistanceMetricTest
      COMMAND ITK-StatisticsTestDriver itkManhattanDistanceMetricTest)
