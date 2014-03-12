project(AnalyzeObjectMap)

set(AnalyzeObjectMapLibrary
    itkAnalyzeObjectEntry.cxx
    itkAnalyzeObjectEntry.h
    itkAnalyzeObjectMap.h)

add_library(AnalyzeObjectMap  ${AnalyzeObjectMapLibrary})
