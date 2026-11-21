if(ITK_WRAP_PYTHON)
  itk_python_add_test(
    NAME itkPointsLocatorPythonTest
    COMMAND
      ${CMAKE_CURRENT_SOURCE_DIR}/itkPointsLocatorTest.py
  )

  list(FIND ITK_WRAP_IMAGE_DIMS 3 wrap_3_index)
  if(ITKBridgeNumPy_ENABLED AND wrap_3_index GREATER -1)
    itk_python_add_test(
      NAME itkDisplacementFieldTransformParametersAdaptorPythonTest
      COMMAND
        ${CMAKE_CURRENT_SOURCE_DIR}/itkDisplacementFieldTransformParametersAdaptorTest.py
    )
  endif()
endif()
