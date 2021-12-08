set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)

# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonMeanImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonMeanImageFilterTest.png
      DATA{Baseline/PythonMeanImageFilterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonMeanImageFilterTest.png
      3
    )

  itk_python_add_test(NAME PythonMeanImageFilterFunctionCallTest
    TEST_DRIVER_ARGS
      --compare
      ${ITK_TEST_OUTPUT_DIR}/PythonMeanImageFilterFunctionCallTest.png
      DATA{Baseline/PythonMeanImageFilterFunctionCallTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilterFunctionCallTest.py
      DATA{${test_input_dir}/cthead1UC.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonMeanImageFilterFunctionCallTest.png
      3
    )

  itk_python_add_test(NAME PythonMedianImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonMedianImageFilterTest.png
      DATA{Baseline/PythonMedianImageFilterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilterTest.py
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonMedianImageFilterTest.png
      3
    )
  if(ITK_WRAP_DOC)
  itk_python_add_test(NAME PythonMedianImageFilterFunctionalDocumentationTest
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilterFunctionalDocumentationTest.py
  )
  endif()
  itk_python_add_test(NAME PythonAutoPipelineTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonAutoPipelineTest.png
      DATA{Baseline/PythonMedianImageFilterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/PythonAutoPipelineTest.py
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonAutoPipelineTest.png
      3
    )
endif()
