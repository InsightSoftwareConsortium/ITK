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
    DATA{Baseline/C0004255.mha}
    ${ITK_TEST_OUTPUT_DIR}/C0004255.mha
    itkScancoImageIOTest
    DATA{Input/C0004255.ISQ}
    ${ITK_TEST_OUTPUT_DIR}/C0004255.mha
)

itk_add_test(
  NAME itkScancoImageIOTest2
  COMMAND
    IOScancoTestDriver
    itkScancoImageIOTest2
    DATA{Input/C0004255.ISQ}
    ${ITK_TEST_OUTPUT_DIR}/C0003665.isq
    1
)

itk_add_test(
  NAME itkScancoImageIOTest3
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{Baseline/C0004255.mha}
    ${ITK_TEST_OUTPUT_DIR}/C0004255Test3.mha
    itkScancoImageIOTest2
    DATA{Input/C0004255.ISQ}
    ${ITK_TEST_OUTPUT_DIR}/C0004255Test3.mha
)
set_property(
  TEST
    itkScancoImageIOTest3
  APPEND
  PROPERTY
    DEPENDS
      itkScancoImageIOTest2
)
