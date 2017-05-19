itk_module_test()
set(
  IOScancoTests
  itkScancoImageIOTest.cxx
  itkScancoImageIOTest2.cxx
)

createtestdriver(IOScanco  "${IOScanco-Test_LIBRARIES}" "${IOScancoTests}")

itk_add_test(
  NAME itkScancoImageIOTest
  COMMAND
    IOScancoTestDriver
    --compare
    ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/C0003665.mha
    ${ITK_TEST_OUTPUT_DIR}/C0003665.mha
    itkScancoImageIOTest
    /home/matt/data/BPBPHumerus/00004180-phantom-2014.0620/C0003665.ISQ
    ${ITK_TEST_OUTPUT_DIR}/C0003665.mha
)

itk_add_test(
  NAME itkScancoImageIOTest2
  COMMAND
    IOScancoTestDriver
    itkScancoImageIOTest2
    /home/matt/data/BPBPHumerus/00004180-phantom-2014.0620/C0003665.ISQ
    ${ITK_TEST_OUTPUT_DIR}/C0003665.isq
    1
)

itk_add_test(
  NAME itkScancoImageIOTest3
  COMMAND
    IOScancoTestDriver
    --compare
    ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/C0003665.mha
    ${ITK_TEST_OUTPUT_DIR}/C0003665Test3.mha
    itkScancoImageIOTest2
    ${ITK_TEST_OUTPUT_DIR}/C0003665.isq
    ${ITK_TEST_OUTPUT_DIR}/C0003665Test3.mha
)
set_property(
  TEST
    itkScancoImageIOTest3
  APPEND
  PROPERTY
    DEPENDS
      itkScancoImageIOTest2
)
