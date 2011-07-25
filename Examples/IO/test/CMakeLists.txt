set(BASELINE ${ITK_DATA_ROOT}/Baseline/Registration)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

if(ITK_VISIBLEHUMAN_DATA_ROOT)
  itk_add_test(NAME VisibleHumanStreamReadWriteTest
    COMMAND ${ITK_TEST_DRIVER}
   $<TARGET_FILE:VisibleHumanStreamReadWrite>
    ${ITK_VISIBLEHUMAN_DATA_ROOT}/Male/Fullcolor/fullbody/
    ${TEMP}/VisibleHumanStreamReadWrite.mha )
  itk_add_test(NAME VisibleHumanPasteWriteTest
    COMMAND ${ITK_TEST_DRIVER}
   $<TARGET_FILE:VisibleHumanPasteWrite>
    ${TEMP}/VisibleHumanStreamReadWrite.mha
    ${TEMP}/VisibleHumanPasteWrite.mha)
  # this test must be run sequentially, be cause the output of the first is used by the second
  set_tests_properties(
    VisibleHumanStreamReadWriteTest
    PROPERTIES
    RUN_SERIAL 1
    )
  set_tests_properties(
    VisibleHumanPasteWriteTest
    PROPERTIES
    DEPENDS VisibleHumanStreamReadWriteTest
    )
endif(ITK_VISIBLEHUMAN_DATA_ROOT)
