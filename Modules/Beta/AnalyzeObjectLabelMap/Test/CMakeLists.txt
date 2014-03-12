cmake_minimum_required(VERSION 2.6)

project(AnalyzeObjectMapTest)

find_package(ITK REQUIRED)
if(ITK_FOUND)
  include(${ITK_USE_FILE})
endif(ITK_FOUND)

include_directories(${AnalyzeObjectMap_SOURCE_DIR})
include_directories(${AnalyzeObjectMapIO_SOURCE_DIR})

file(MAKE_DIRECTORY ${ANALYZE_OBJECT_MAP_BINARY_DIR}/Temporary)
set(TEST_DATA_ROOT     ${AnalyzeObjectMapTest_SOURCE_DIR}/Data/Input)
set(TESTING_OUTPUT_DIR ${ANALYZE_OBJECT_MAP_BINARY_DIR}/Temporary)

include_directories( ${AnalyzeObjectMapTest_SOURCE_DIR} )

add_executable( AnalyzeObjectMapTest AnalyzeObjectMapTest.cxx )
target_link_libraries( AnalyzeObjectMapTest ${ITK_LIBRARIES}  AnalyzeObjectMapIO AnalyzeObjectMap)

add_test( ImageIOStreamingAnalyzeObjectLabelMapTest01
   ${EXECUTABLE_OUTPUT_PATH}/AnalyzeObjectMapTest
   ${TEST_DATA_ROOT}/test.obj
   ${TESTING_OUTPUT_DIR}/objectLabelTest.obj
   ${TEST_DATA_ROOT}/2dtest.nii
   ${TESTING_OUTPUT_DIR}/creatingObject.obj
   ${TESTING_OUTPUT_DIR}/circle.obj
   ${TESTING_OUTPUT_DIR}/blankImage.obj
   ${TESTING_OUTPUT_DIR}/OneDimensionImage.obj
   )

