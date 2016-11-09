set(ITKLabelMap_SRCS
  itkGeometryUtilities.cxx
  )
### generating libraries
itk_module_add_library( ITKLabelMap ${ITKLabelMap_SRCS})
