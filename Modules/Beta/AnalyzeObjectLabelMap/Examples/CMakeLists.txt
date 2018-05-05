cmake_minimum_required(VERSION 3.10.2)

project(AnalyzeObjectMapExamples)

find_package(ITK REQUIRED)
if(ITK_FOUND)
  include(${ITK_USE_FILE})
endif()

#find_package( VTK REQUIRED )
#if( VTK_FOUND )
#    include( ${USE_VTK_FILE} )
#endif()


include_directories(${AnalyzeObjectMap_SOURCE_DIR})
include_directories(${AnalyzeObjectMapIO_SOURCE_DIR})

set(TEST_DATA_ROOT     ${AnalyzeObjectMapExamples_SOURCE_DIR}/Data/Input)
set(TESTING_OUTPUT_DIR ${AnalyzeObjectMapExamples_BINARY_DIR}/Temporary)

include_directories( ${AnalyzeObjectMapExamples_SOURCE_DIR} )

add_executable( CreatingObjects CreatingObjects.cxx )
target_link_libraries( CreatingObjects ${ITK_LIBRARIES} AnalyzeObjectMapIO AnalyzeObjectMap)

add_executable( DisplayingObjectMaps DisplayingObjectMaps.cxx )
target_link_libraries( DisplayingObjectMaps ${ITK_LIBRARIES} AnalyzeObjectMapIO AnalyzeObjectMap) #vtkRendering vtkImaging vtkIO vtkCommon)

add_executable( PickOneObjectEntry PickOneObjectEntry.cxx )
target_link_libraries( PickOneObjectEntry ${ITK_LIBRARIES} AnalyzeObjectMapIO AnalyzeObjectMap)


