set(
  ITKStatistics_SRCS
  itkChiSquareDistribution.cxx
  itkDecisionRule.cxx
  itkDenseFrequencyContainer2.cxx
  itkExpectationMaximizationMixtureModelEstimator.cxx
  itkGaussianDistribution.cxx
  itkHistogramToRunLengthFeaturesFilter.cxx
  itkHistogramToTextureFeaturesFilter.cxx
  itkMaximumDecisionRule.cxx
  itkMaximumRatioDecisionRule.cxx
  itkMinimumDecisionRule.cxx
  itkNormalVariateGenerator.cxx
  itkProbabilityDistribution.cxx
  itkSparseFrequencyContainer2.cxx
  itkTDistribution.cxx
)

itk_module_add_library(ITKStatistics ${ITKStatistics_SRCS})
