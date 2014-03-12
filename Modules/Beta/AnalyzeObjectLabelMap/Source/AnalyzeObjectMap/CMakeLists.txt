PROJECT(AnalyzeObjectMap)

SET(AnalyzeObjectMapLibrary
itkAnalyzeObjectEntry.cxx
itkAnalyzeObjectEntry.h
itkAnalyzeObjectMap.h)

ADD_LIBRARY(AnalyzeObjectMap  ${AnalyzeObjectMapLibrary})