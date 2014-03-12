PROJECT(AnalyzeObjectMapIO)

INCLUDE_DIRECTORIES(${AnalyzeObjectMap_SOURCE_DIR})

SET(AnalyzeObjectMapIOLibrary 
	itkAnalyzeObjectLabelMapImageIO.cxx 
	itkAnalyzeObjectLabelMapImageIOFactory.cxx)


ADD_LIBRARY(AnalyzeObjectMapIO ${AnalyzeObjectMapIOLibrary})

TARGET_LINK_LIBRARIES(AnalyzeObjectMapIO 
AnalyzeObjectMap
)
