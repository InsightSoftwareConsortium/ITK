set(ITKLabelMap_SRCS
itkGeometryUtilities.cxx
)
### generating libraries
add_library( ITKLabelMap ${ITKLabelMap_SRCS})
itk_module_link_dependencies()
itk_module_target(ITKLabelMap)
