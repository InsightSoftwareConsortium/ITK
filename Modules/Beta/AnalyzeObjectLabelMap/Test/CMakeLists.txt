cmake_minimum_required(VERSION 2.6)

PROJECT(AnalyzeObjectMapTest)

FIND_PACKAGE(ITK REQUIRED)
IF(ITK_FOUND)
  INCLUDE(${ITK_USE_FILE})
ENDIF(ITK_FOUND)

INCLUDE_DIRECTORIES(${AnalyzeObjectMap_SOURCE_DIR})
INCLUDE_DIRECTORIES(${AnalyzeObjectMapIO_SOURCE_DIR})

FILE(MAKE_DIRECTORY ${ANALYZE_OBJECT_MAP_BINARY_DIR}/Temporary)
SET(TEST_DATA_ROOT     ${AnalyzeObjectMapTest_SOURCE_DIR}/Data/Input)
SET(TESTING_OUTPUT_DIR ${ANALYZE_OBJECT_MAP_BINARY_DIR}/Temporary)

INCLUDE_DIRECTORIES( ${AnalyzeObjectMapTest_SOURCE_DIR} )

ADD_EXECUTABLE( AnalyzeObjectMapTest AnalyzeObjectMapTest.cxx )
TARGET_LINK_LIBRARIES( AnalyzeObjectMapTest ITKIO  AnalyzeObjectMapIO AnalyzeObjectMap)

ADD_TEST( ImageIOStreamingAnalyzeObjectLabelMapTest01
   ${EXECUTABLE_OUTPUT_PATH}/AnalyzeObjectMapTest
   ${TEST_DATA_ROOT}/test.obj
   ${TESTING_OUTPUT_DIR}/objectLabelTest.obj
   ${TEST_DATA_ROOT}/2dtest.nii
   ${TESTING_OUTPUT_DIR}/creatingObject.obj
   ${TESTING_OUTPUT_DIR}/circle.obj
   ${TESTING_OUTPUT_DIR}/blankImage.obj
   ${TESTING_OUTPUT_DIR}/OneDimensionImage.obj
   )

