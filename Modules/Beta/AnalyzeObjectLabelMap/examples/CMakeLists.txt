cmake_minimum_required(VERSION 3.10.2)

project(AnalyzeObjectMapExamples)

find_package(ITK 5.1 REQUIRED)
if(ITK_FOUND)
 include(${ITK_USE_FILE})
endif()

#find_package( VTK REQUIRED )
#if( VTK_FOUND )
#  include( ${USE_VTK_FILE} )
#endif()


include_directories(${AnalyzeObjectMap_SOURCE_DIR})
include_directories(${AnalyzeObjectMapIO_SOURCE_DIR})

set(TEST_DATA_ROOT     ${AnalyzeObjectMapExamples_SOURCE_DIR}/Data/Input)
set(TESTING_OUTPUT_DIR ${AnalyzeObjectMapExamples_BINARY_DIR}/Temporary)

include_directories( ${AnalyzeObjectMapExamples_SOURCE_DIR} )

add_executable( CreatingObjects CreatingObjects.cxx )
target_link_libraries( CreatingObjects ${AnalyzeObjectMapIO_LIBRARIES} ${ITK_LIBRARIES})

add_executable( DisplayingObjectMaps DisplayingObjectMaps.cxx )
target_link_libraries( DisplayingObjectMaps ${AnalyzeObjectMapIO_LIBRARIES} ${ITK_LIBRARIES})

add_executable( PickOneObjectEntry PickOneObjectEntry.cxx )
target_link_libraries( PickOneObjectEntry ${AnalyzeObjectMapIO_LIBRARIES} ${ITK_LIBRARIES})


