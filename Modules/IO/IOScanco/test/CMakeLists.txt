itk_module_test()
set(
  IOScancoTests
  itkScancoImageIOTest.cxx
  itkScancoImageIOTest2.cxx
  itkScancoImageIOTest3.cxx
)

createtestdriver(IOScanco  "${IOScanco-Test_LIBRARIES}" "${IOScancoTests}")

itk_add_test(
  NAME itkScancoImageIOISQConvertTest
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
  NAME itkScancoImageIOAIMConvertTest
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{Baseline/AIMIOTestImage.mha}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestImage.mha
    itkScancoImageIOTest
    DATA{Input/AIMIOTestImage.AIM}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestImage.mha
)

itk_add_test(
  NAME itkScancoImageIOISQHeaderCheckTest
  COMMAND
    IOScancoTestDriver
    itkScancoImageIOTest2
    DATA{Input/C0004255.ISQ}
    ${ITK_TEST_OUTPUT_DIR}/C0003665.isq
    1
)

itk_add_test(
  NAME itkScancoImageIOAIMHeaderCheckTest
  COMMAND
    IOScancoTestDriver
    itkScancoImageIOTest3
    DATA{Input/AIMIOTestImage.AIM}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestImage2.isq
    1
)

itk_add_test(
  NAME itkScancoImageIOISQHeaderCheckTest2
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{Baseline/C0004255.mha}
    ${ITK_TEST_OUTPUT_DIR}/C0004255Test2.mha
    itkScancoImageIOTest2
    DATA{Input/C0004255.ISQ}
    ${ITK_TEST_OUTPUT_DIR}/C0004255Test2.mha
)

itk_add_test(
  NAME itkScancoImageIOISQHeaderWriteTest2
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{Baseline/C0004255.mha}
    ${ITK_TEST_OUTPUT_DIR}/C0004255Test2.isq
    itkScancoImageIOTest2
    DATA{Input/C0004255.ISQ}
    ${ITK_TEST_OUTPUT_DIR}/C0004255Test2.isq
)

itk_add_test(
  NAME itkScancoImageIOAIMHeaderCheckTest2
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{Baseline/AIMIOTestImage.mha}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestImageTest3.mha
    itkScancoImageIOTest3
    DATA{Input/AIMIOTestImage.AIM}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestImageTest3.mha
)
set_property(
  TEST
    itkScancoImageIOAIMHeaderCheckTest2
  APPEND
  PROPERTY
    DEPENDS
      itkScancoImageIOAIMHeaderCheckTest
)

itk_add_test(
  NAME itkScancoImageIOISQReadWritten
  COMMAND
    IOScancoTestDriver
    itkScancoImageIOTest2
    ${ITK_TEST_OUTPUT_DIR}/C0004255Test2.isq
    ${ITK_TEST_OUTPUT_DIR}/ISQWrite.ISQ
)
set_property(
  TEST
    itkScancoImageIOISQReadWritten
  APPEND
  PROPERTY
    DEPENDS
      itkScancoImageIOISQHeaderWriteTest2
)

itk_add_test(
  NAME itkScancoImageIOAIMWriteTest
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{Baseline/AIMIOTestImage.mha}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestWrite.aim
    itkScancoImageIOTest3
    DATA{Input/AIMIOTestImage.AIM}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestWrite.aim
)

itk_add_test(
  NAME itkScancoImageIOAIMReadWrittenTest
  COMMAND
    IOScancoTestDriver
    itkScancoImageIOTest3
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestWrite.aim
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestWrite2.aim
)

set_property(
  TEST
    itkScancoImageIOAIMReadWrittenTest
  APPEND
  PROPERTY
    DEPENDS
      itkScancoImageIOAIMWriteTest
)

itk_add_test(
  NAME itkScancoImageIOAIM030FloatReadHeader
  COMMAND
    IOScancoTestDriver
    --compare
    DATA{Input/AIMIOTestImageFloatv030.AIM}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestWritev030.aim
    itkScancoImageIOTest3
    DATA{Input/AIMIOTestImageFloatv030.AIM}
    ${ITK_TEST_OUTPUT_DIR}/AIMIOTestWritev030.aim
    0
    "AIMDATA_V030   "
)
