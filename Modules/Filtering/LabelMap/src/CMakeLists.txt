set(ITKLabelMap_SRCS
  itkGeometryUtilities.cxx
  itkMergeLabelMapFilter.cxx
  )
### generating libraries
itk_module_add_library(ITKLabelMap ${ITKLabelMap_SRCS})
