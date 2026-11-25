set(
  ITKOptimizersv4_SRCS
  itkAmoebaOptimizerv4.cxx
  itkLBFGS2Optimizerv4.cxx
  itkLBFGSBOptimizerv4.cxx
  itkLBFGSOptimizerv4.cxx
  itkLBFGSOptimizerv4Base.cxx
  itkObjectToObjectMetricBase.cxx
  itkObjectToObjectOptimizerBase.cxx
  itkRegistrationParameterScalesEstimator.cxx
  itkSingleValuedNonLinearVnlOptimizerv4.cxx
  itkSingleValuedVnlCostFunctionAdaptorv4.cxx
)

itk_module_add_library(ITKOptimizersv4 ${ITKOptimizersv4_SRCS})
target_link_libraries(ITKOptimizersv4 PUBLIC ${ITKMetricsv4_LIBRARIES})
