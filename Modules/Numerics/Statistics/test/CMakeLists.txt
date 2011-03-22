itk_module_test()
set(ITK-StatisticsTests
itkKalmanLinearEstimatorTest.cxx
itkStatisticsHeaderTest.cxx
itkMaximumDecisionRuleTest.cxx
itkMinimumDecisionRuleTest.cxx
itkMaximumRatioDecisionRuleTest.cxx
itkHistogramTest.cxx
itkHistogramToTextureFeaturesFilterTest.cxx
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
