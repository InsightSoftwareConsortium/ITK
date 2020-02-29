set(AnalyzeObjectLabelMap_SRC
  itkAnalyzeObjectLabelMapImageIO.cxx
  itkAnalyzeObjectLabelMapImageIOFactory.cxx
  itkAnalyzeObjectEntry.cxx)

add_library(AnalyzeObjectLabelMap ${AnalyzeObjectLabelMap_SRC})

target_link_libraries(AnalyzeObjectLabelMap ${ITKIOImageBase_LIBRARIES})
itk_module_target(AnalyzeObjectLabelMap)
