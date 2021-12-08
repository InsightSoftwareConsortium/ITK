# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonFlatStructuringElementBall
    TEST_DRIVER_ARGS
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElementTest.py
      Ball
      5
    )
  itk_python_add_test(NAME PythonFlatStructuringElementBox
    TEST_DRIVER_ARGS
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/FlatStructuringElementTest.py
      Box
      5
    )
  itk_python_add_test(NAME PythonGrayscaleDilateImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonGrayscaleDilateImageFilterTest.png
      DATA{Baseline/PythonGrayscaleDilateImageFilterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleDilateImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonGrayscaleDilateImageFilterTest.png
      5
    )
  itk_python_add_test(NAME PythonGrayscaleErodeImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonGrayscaleErodeImageFilterTest.png
      DATA{Baseline/PythonGrayscaleErodeImageFilterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/GrayscaleErodeImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonGrayscaleErodeImageFilterTest.png
      5
    )
  itk_python_add_test(NAME PythonBoxGrayscaleDilateImageFilterTest
    TEST_DRIVER_ARGS
      --compare
      ${ITK_TEST_OUTPUT_DIR}/PythonBoxGrayscaleDilateImageFilterTest.png
      DATA{Baseline/PythonBoxGrayscaleDilateImageFilterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/BoxGrayscaleDilateImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonBoxGrayscaleDilateImageFilterTest.png
    )
endif()
