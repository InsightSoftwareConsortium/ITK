cmake_minimum_required(VERSION 3.10.2)

project(AnalyzeObjectLabelMapExamples
  VERSION 5.1.0       #Version should track with ITK
  LANGUAGES CXX)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK 5.1 REQUIRED)
  if(ITK_FOUND)
   include(${ITK_USE_FILE})
  endif()
endif()
#find_package( VTK REQUIRED )
#if( VTK_FOUND )
#  include( ${USE_VTK_FILE} )
#endif()

#set(TEST_DATA_ROOT     ${AnalyzeObjectLabelMapExamples_SOURCE_DIR}/Data/Input)
#set(TESTING_OUTPUT_DIR ${AnalyzeObjectLabelMapExamples_BINARY_DIR}/Temporary)

include_directories(${AnalyzeObjectLabelMap_SOURCE_DIR})
include_directories(${AnalyzeObjectLabelMapExamples_SOURCE_DIR})

add_executable( CreatingObjects CreatingObjects.cxx )
target_link_libraries( CreatingObjects ${AnalyzeObjectLabelMap_LIBRARIES} ${ITK_LIBRARIES})

add_executable( DisplayingObjectMaps DisplayingObjectMaps.cxx )
target_link_libraries( DisplayingObjectMaps ${AnalyzeObjectLabelMap_LIBRARIES} ${ITK_LIBRARIES})

add_executable( PickOneObjectEntry PickOneObjectEntry.cxx )
target_link_libraries( PickOneObjectEntry ${AnalyzeObjectLabelMap_LIBRARIES} ${ITK_LIBRARIES})
