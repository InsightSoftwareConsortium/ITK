# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonBinaryDilateImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonBinaryDilateImageFilterTest.png DATA{Baseline/PythonBinaryDilateImageFilterTest.png}
    COMMAND BinaryDilateImageFilterTest.py
    DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonBinaryDilateImageFilterTest.png
      5
    )
  itk_python_add_test(NAME PythonBinaryErodeImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonBinaryErodeImageFilterTest.png
      DATA{Baseline/PythonBinaryErodeImageFilterTest.png}
    COMMAND BinaryErodeImageFilterTest.py
    DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonBinaryErodeImageFilterTest.png
      5
    )
endif()
