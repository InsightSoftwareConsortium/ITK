set(ITKLabelMap_SRCS
itkGeometryUtilities.cxx
)
### generating libraries
add_library( ITKLabelMap ${ITKLabelMap_SRCS})
target_link_libraries(ITKLabelMap ${ITKStatistics_LIBRARIES})
itk_module_target(ITKLabelMap)
