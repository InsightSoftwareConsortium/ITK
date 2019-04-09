# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonBinaryThresholdImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonBinaryThresholdImageFilterTest.png
      DATA{Baseline/PythonBinaryThresholdImageFilterTest.png}
    COMMAND BinaryThresholdImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonBinaryThresholdImageFilterTest.png
      50 150
      0 255
    )
  itk_python_add_test(NAME PythonThresholdImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonThresholdImageFilterTest.png
      DATA{Baseline/PythonThresholdImageFilterTest.png}
    COMMAND ThresholdImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonThresholdImageFilterTest.png
      150 150
    )
endif()
