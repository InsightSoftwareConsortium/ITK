# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonCastImageFilterTest
    TEST_DRIVER_ARGS
    --compare ${ITK_TEST_OUTPUT_DIR}/PythonCastImageFilterTest.png DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    COMMAND CastImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonCastImageFilterTest.png
    )
endif()
