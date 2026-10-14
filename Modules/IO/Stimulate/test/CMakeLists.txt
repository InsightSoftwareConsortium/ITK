itk_module_test()
set(
  ITKIOStimulateTests
  itkStimulateImageIOTest.cxx
  itkStimulateImageIOInvalidHeaderTest.cxx
  itkStimulateImageIOTest2.cxx
)

createtestdriver(ITKIOStimulate "${ITKIOStimulate-Test_LIBRARIES}" "${ITKIOStimulateTests}")

itk_add_test(
  NAME itkStimulateImageIOTest
  COMMAND
    ITKIOStimulateTestDriver
    itkStimulateImageIOTest
    ${ITK_TEST_OUTPUT_DIR}/test1.spr
    ${ITK_TEST_OUTPUT_DIR}/test2.spr
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/test1.spr
    ${ITK_TEST_OUTPUT_DIR}/test2.spr
)
itk_add_test(
  NAME itkStimulateImageIOInvalidHeaderTest
  COMMAND
    ITKIOStimulateTestDriver
    itkStimulateImageIOInvalidHeaderTest
    ${ITK_TEST_OUTPUT_DIR}
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/stimulate-oversized-num-dim.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-huge-num-dim.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-malformed-num-dim.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-missing-dim.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-short-dim.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-short-origin.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-short-fov.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-short-interval.spr
    ${ITK_TEST_OUTPUT_DIR}/stimulate-zero-dim.spr
)
itk_add_test(
  NAME itkStimulateImageIOTest2
  COMMAND
    ITKIOStimulateTestDriver
    itkStimulateImageIOTest2
    DATA{${ITK_DATA_ROOT}/Input/BigEndian.spr,BigEndian.img}
)
