set(ITKHighDimensionalOptimizers_SRC
  itkObjectToObjectMetric.cxx
  itkObjectToObjectOptimizerBase.cxx
  itkGradientDescentObjectOptimizerBase.cxx
  itkGradientDescentObjectOptimizer.cxx
)

add_library(ITKHighDimensionalOptimizers ${ITKHighDimensionalOptimizers_SRC})
target_link_libraries(ITKHighDimensionalOptimizers
  ${ITKHighDimensionalMetrics_LIBRARIES}
  ${ITKOptimizers_LIBRARIES}
  )
itk_module_target(ITKHighDimensionalOptimizers)
