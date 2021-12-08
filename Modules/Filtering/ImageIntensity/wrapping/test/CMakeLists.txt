# some tests will fail if dim=2 and unsigned short are not wrapped
if(ITK_WRAP_PYTHON)
  list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
  if(ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
    itk_python_add_test(NAME PythonSigmoidImageFilterTest
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/PythonSigmoidImageFilterTest.png
        DATA{Baseline/PythonSigmoidImageFilterTest.png}
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/SigmoidImageFilterTest.py
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/PythonSigmoidImageFilterTest.png
        10 240 10 170
      )
      itk_python_add_test(NAME PythonIntensityWindowingImageFilterTest
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkIntensityWindowingImageFilterTest.py
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        )
  endif()
  itk_python_expression_add_test(NAME itkSymmetricEigenAnalysisImageFilterPythonTest
    EXPRESSION "filt = itk.SymmetricEigenAnalysisImageFilter.New()")
  itk_python_add_test(NAME itkImageFilterNumPyInputsTest
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkImageFilterNumPyInputsTest.py)
endif()
