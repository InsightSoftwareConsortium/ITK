set(ITKOptimizersv4_SRC
  itkObjectToObjectMetricBase.cxx
  itkObjectToObjectOptimizerBase.cxx
  itkGradientDescentOptimizerBasev4.cxx
  itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader.cxx
  itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader.cxx
  itkGradientDescentOptimizerv4.cxx
  itkGradientDescentLineSearchOptimizerv4.cxx
  itkConjugateGradientLineSearchOptimizerv4.cxx
  itkMultiStartOptimizerv4.cxx
  itkMultiGradientOptimizerv4.cxx
  itkQuasiNewtonOptimizerv4.cxx
  itkQuasiNewtonOptimizerv4EstimateNewtonStepThreader.cxx
)

add_library(ITKOptimizersv4 ${ITKOptimizersv4_SRC})
target_link_libraries(ITKOptimizersv4
  ${ITKMetricsv4_LIBRARIES}
  ${ITKOptimizers_LIBRARIES}
  )
itk_module_target(ITKOptimizersv4)
