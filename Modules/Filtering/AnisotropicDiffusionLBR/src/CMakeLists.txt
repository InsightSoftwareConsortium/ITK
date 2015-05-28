set(${itk-module}_SRCS
CoherenceEnhancingDiffusion.cxx
  )

add_library(${itk-module} ${${itk-module}_SRCS})

target_link_libraries(${itk-module}  ${${itk-module}_LIBRARIES})
itk_module_target(${itk-module})
