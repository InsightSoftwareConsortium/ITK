itk_module_test()
set(ITKIOGETests
itkGEImageIOTest.cxx
)

CreateTestDriver(ITKIOGE  "${ITKIOGE-Test_LIBRARIES}" "${ITKIOGETests}")

itk_add_test(NAME itkGE4
      COMMAND ITKIOGETestDriver
    --compareCoordinateTolerance 1e-5  # Fails on Windows otherwise
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/19771.002.001.mha}
              ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE4 DATA{${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001,REGEX:19771\\.002\\.00[1-6]} ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha)
itk_add_test(NAME itkGE5
      COMMAND ITKIOGETestDriver
    --compareCoordinateTolerance 1e-5  # Fails on Windows otherwise
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/113766.003.001.mha}
              ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE5 DATA{${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001,REGEX:113766\\.003\\.00[1-6]} ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha)
itk_add_test(NAME itkGE5Test2
      COMMAND ITKIOGETestDriver
    --compare DATA{Baseline/c_vf1122.fre.mha}
              ${ITK_TEST_OUTPUT_DIR}/c_vf1122.fre.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE5 DATA{Input/c_vf1122.fre} ${ITK_TEST_OUTPUT_DIR}/c_vf1122.fre.mha)
itk_add_test(NAME itkGE5Test3
      COMMAND ITKIOGETestDriver
    --compare DATA{Baseline/c_vf1122.fre.mha}
              ${ITK_TEST_OUTPUT_DIR}/c_vf1123.fre.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE5 DATA{Input/c_vf1123.fre} ${ITK_TEST_OUTPUT_DIR}/c_vf1123.fre.mha)
itk_add_test(NAME itkGE5Test4
      COMMAND ITKIOGETestDriver
    --compare DATA{Baseline/c_vf1210.fre.mha}
              ${ITK_TEST_OUTPUT_DIR}/c_vf1210.fre.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE5 DATA{Input/c_vf1210.fre} ${ITK_TEST_OUTPUT_DIR}/c_vf1210.fre.mha)
itk_add_test(NAME itkGE5Test5
      COMMAND ITKIOGETestDriver
    --compare DATA{Baseline/c_vf1210.fre.mha}
              ${ITK_TEST_OUTPUT_DIR}/c_vf1211.fre.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE5 DATA{Input/c_vf1211.fre} ${ITK_TEST_OUTPUT_DIR}/c_vf1211.fre.mha)
itk_add_test(NAME itkGEAdw
      COMMAND ITKIOGETestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/I.001.mha}
              ${ITK_TEST_OUTPUT_DIR}/I.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GEAdw DATA{${ITK_DATA_ROOT}/Input/test_geadw/I.001,REGEX:I\\.00[1-6]} ${ITK_TEST_OUTPUT_DIR}/I.001.mha)
itk_add_test(NAME itkSiemens
      COMMAND ITKIOGETestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/3868-2-100.mha}
              ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true Siemens DATA{${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima,REGEX:3868-2-10[0-5]\\.ima} ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha)
itk_add_test(NAME itkGE4Fail
      COMMAND ITKIOGETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GE4 DATA{${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001,REGEX:113766\\.003\\.00[1-6]})
itk_add_test(NAME itkGE5Fail
      COMMAND ITKIOGETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GE5 DATA{${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001,REGEX:19771\\.002\\.00[1-6]})
itk_add_test(NAME itkGEAdwFail
      COMMAND ITKIOGETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GEAdw DATA{${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima,REGEX:3868-2-10[0-5]\\.ima})
itk_add_test(NAME itkSiemensFail
      COMMAND ITKIOGETestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false Siemens DATA{${ITK_DATA_ROOT}/Input/test_geadw/I.001,,REGEX:I\\.00[1-6]})
