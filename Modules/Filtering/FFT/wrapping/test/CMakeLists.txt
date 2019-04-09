list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonFFTImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonFFTImageFilterRealTest.png DATA{Baseline/PythonFFTImageFilterRealTest.png}
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonFFTImageFilterImaginaryTest.png DATA{Baseline/PythonFFTImageFilterImaginaryTest.png}
    COMMAND FFTImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonFFTImageFilterRealTest.png
      ${ITK_TEST_OUTPUT_DIR}/PythonFFTImageFilterImaginaryTest.png
  )
endif()
