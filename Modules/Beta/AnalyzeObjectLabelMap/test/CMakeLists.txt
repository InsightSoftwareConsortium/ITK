itk_module_test()
set(AnalyzeObjectLabelMapTests_SRCS AnalyzeObjectMapTest.cxx )

set(TEST_DATA_ROOT     ${CMAKE_CURRENT_LIST_DIR}/Data/Input)
set(TESTING_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})


CreateTestDriver(AnalyzeObjectLabelMap
  "${AnalyzeObjectLabelMap-Test_LIBRARIES}"
  "${AnalyzeObjectLabelMapTests_SRCS}")

itk_add_test(NAME AnalyzeObjectLabelMapTest1
  COMMAND AnalyzeObjectLabelMapTestDriver
  AnalyzeObjectMapTest
  ${TEST_DATA_ROOT}/test.obj
  ${TESTING_OUTPUT_DIR}/objectLabelTest.obj
  ${TEST_DATA_ROOT}/2dtest.nii
  ${TESTING_OUTPUT_DIR}/creatingObject.obj
  ${TESTING_OUTPUT_DIR}/circle.obj
  ${TESTING_OUTPUT_DIR}/blankImage.obj
  ${TESTING_OUTPUT_DIR}/OneDimensionImage.obj
  )

