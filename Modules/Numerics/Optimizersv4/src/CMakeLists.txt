set(ITKOptimizersv4_SRC
  itkSingleValuedNonLinearVnlOptimizerv4.cxx
  itkSingleValuedVnlCostFunctionAdaptorv4.cxx
  itkLBFGSOptimizerv4.cxx
  itkLBFGSBOptimizerv4.cxx
  )

add_library(ITKOptimizersv4 ${ITKOptimizersv4_SRC})
target_link_libraries(ITKOptimizersv4
  ${ITKMetricsv4_LIBRARIES}
  ${ITKOptimizers_LIBRARIES}
  )
itk_module_target(ITKOptimizersv4)
