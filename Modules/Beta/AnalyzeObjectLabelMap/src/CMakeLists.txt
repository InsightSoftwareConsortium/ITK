set(AnalyzeObjectMapIO_SRC
  itkAnalyzeObjectLabelMapImageIO.cxx
  itkAnalyzeObjectLabelMapImageIOFactory.cxx
  itkAnalyzeObjectEntry.cxx)

add_library(AnalyzeObjectMapIO ${AnalyzeObjectMapIO_SRC})

target_link_libraries(AnalyzeObjectMapIO ${ITKIOImageBase_LIBRARIES}
)
itk_module_target(AnalyzeObjectMapIO)
