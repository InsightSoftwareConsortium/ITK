project(AnalyzeObjectMapIO)

include_directories(${AnalyzeObjectMap_SOURCE_DIR})

set(AnalyzeObjectMapIOLibrary
	itkAnalyzeObjectLabelMapImageIO.cxx
	itkAnalyzeObjectLabelMapImageIOFactory.cxx)


add_library(AnalyzeObjectMapIO ${AnalyzeObjectMapIOLibrary})

target_link_libraries(AnalyzeObjectMapIO
AnalyzeObjectMap
)
