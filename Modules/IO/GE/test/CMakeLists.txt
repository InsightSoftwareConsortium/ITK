itk_module_test()
set(ITK-IO-GETests
itkGEImageIOTest.cxx
itkIOGEHeaderTest.cxx
)

CreateTestDriver(ITK-IO-GE  "${ITK-IO-GE-Test_LIBRARIES}" "${ITK-IO-GETests}")

itk_add_test(NAME itkIOGEHeaderTest
      COMMAND ITK-IO-GETestDriver itkIOGEHeaderTest)
itk_add_test(NAME itkGE4
      COMMAND ITK-IO-GETestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/19771.002.001.mha
              ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE4 ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001 ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha)
itk_add_test(NAME itkGE5
      COMMAND ITK-IO-GETestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/113766.003.001.mha
              ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE5 ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001 ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha)
itk_add_test(NAME itkGEAdw
      COMMAND ITK-IO-GETestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/I.001.mha
              ${ITK_TEST_OUTPUT_DIR}/I.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GEAdw ${ITK_DATA_ROOT}/Input/test_geadw/I.001 ${ITK_TEST_OUTPUT_DIR}/I.001.mha)
itk_add_test(NAME itkSiemens
      COMMAND ITK-IO-GETestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/3868-2-100.mha
              ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true Siemens ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha)
itk_add_test(NAME itkGE4Fail
      COMMAND ITK-IO-GETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GE4 ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
itk_add_test(NAME itkGE5Fail
      COMMAND ITK-IO-GETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GE5 ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
itk_add_test(NAME itkGEAdwFail
      COMMAND ITK-IO-GETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GEAdw ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)
itk_add_test(NAME itkSiemensFail
      COMMAND ITK-IO-GETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false Siemens ${ITK_DATA_ROOT}/Input/test_geadw/I.001)
