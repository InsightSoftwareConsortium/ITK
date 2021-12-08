list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonCurvatureFlowImageFilterTest
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureFlowImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterTest.png
      5 0.125 1.0
    )
endif()
