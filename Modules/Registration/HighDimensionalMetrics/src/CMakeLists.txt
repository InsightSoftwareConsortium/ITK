set(ITKHighDimensionalMetrics_SRC
  itkObjectToObjectMetric.cxx
)

add_library(ITKHighDimensionalMetrics ${ITKHighDimensionalMetrics_SRC})
target_link_libraries(ITKHighDimensionalMetrics
  ${ITKOptimizers_LIBRARIES}
  )
itk_module_target(ITKHighDimensionalMetrics)
