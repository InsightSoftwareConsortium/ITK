itk_module_test()
set(AnalyzeObjectMapIOTests AnalyzeObjectMapTest.cxx )

set(TEST_DATA_ROOT     ${CMAKE_CURRENT_LIST_DIR}/Data/Input)
set(TESTING_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})


CreateTestDriver(AnalyzeObjectMapIO
  "${AnalyzeObjectMapIO-Test_LIBRARIES}"
  "${AnalyzeObjectMapIOTests}")

itk_add_test(NAME AnalyzeObjectMapIOTest1
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

