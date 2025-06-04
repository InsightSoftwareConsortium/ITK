if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(
    NAME
    itkExtrapolateImageFunctionPythonTest
    EXPRESSION
    "extrapolate = itk.ExtrapolateImageFunction.New()")
  itk_python_expression_add_test(
    NAME
    itkWindowedSincInterpolateImageFunctionPythonTest
    EXPRESSION
    "windowed_sinc = itk.WindowedSincInterpolateImageFunction.New()")
  itk_python_add_test(
    NAME
    itkLinearImageFunctionWrappingTest
    COMMAND
    ${CMAKE_CURRENT_SOURCE_DIR}/itkLinearImageFunctionWrappingTest.py)
endif()
