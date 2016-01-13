set(MinimalPathExtraction_SRC itkIterateNeighborhoodOptimizer.cxx)

add_library(
  MinimalPathExtraction
  ${ITK_LIBRARY_BUILD_TYPE}
  ${MinimalPathExtraction_SRC}
)
itk_module_link_dependencies()
itk_module_target(MinimalPathExtraction)
