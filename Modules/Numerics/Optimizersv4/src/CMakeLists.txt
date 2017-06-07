set(ITKOptimizersv4_SRCS
  itkObjectToObjectOptimizerBase.cxx
  itkSingleValuedNonLinearVnlOptimizerv4.cxx
  itkSingleValuedVnlCostFunctionAdaptorv4.cxx
  itkLBFGSOptimizerv4.cxx
  itkLBFGS2Optimizerv4.cxx
  itkLBFGSBOptimizerv4.cxx
  itkAmoebaOptimizerv4.cxx
  )

itk_module_add_library(ITKOptimizersv4 ${ITKOptimizersv4_SRCS})
target_link_libraries(ITKOptimizersv4 LINK_PUBLIC ${ITKMetricsv4_LIBRARIES})
