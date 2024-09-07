set(AnalyzeObjectLabelMap_SRCS
  itkAnalyzeObjectLabelMapImageIO.cxx
  itkAnalyzeObjectLabelMapImageIOFactory.cxx
  itkAnalyzeObjectEntry.cxx)

itk_module_add_library(AnalyzeObjectLabelMap ${AnalyzeObjectLabelMap_SRCS})

