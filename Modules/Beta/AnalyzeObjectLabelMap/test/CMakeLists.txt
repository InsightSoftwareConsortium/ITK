itk_module_test()

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(TEST_DATA_ROOT     ${CMAKE_CURRENT_LIST_DIR}/Data/Input)
set(TESTING_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})

include_directories( ${AnalyzeObjectMapTest_SOURCE_DIR} )

set(AnalyzObjectMapIOTests AnalyzeObjectMapTest.cxx )

CreateTestDriver(AnalyzeObjectMapIO
  "${AnalyzeObjectMapIO-Test_LIBRARIES}"
  "${AnalyzeObjectMapIOTests}")


add_test( ImageIOStreamingAnalyzeObjectLabelMapTest01
  COMMAND AnalyzeObjectMapIOTestDriver
  AnalyzeObjectMapTest
  ${TEST_DATA_ROOT}/test.obj
  ${TESTING_OUTPUT_DIR}/objectLabelTest.obj
  ${TEST_DATA_ROOT}/2dtest.nii
  ${TESTING_OUTPUT_DIR}/creatingObject.obj
  ${TESTING_OUTPUT_DIR}/circle.obj
  ${TESTING_OUTPUT_DIR}/blankImage.obj
  ${TESTING_OUTPUT_DIR}/OneDimensionImage.obj
  )

