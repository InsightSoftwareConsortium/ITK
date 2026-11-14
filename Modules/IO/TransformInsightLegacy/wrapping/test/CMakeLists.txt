if(ITK_WRAP_PYTHON)
  itk_python_add_test(
    NAME itkBSplineTransformSplineOrderTest
    COMMAND
      ${CMAKE_CURRENT_SOURCE_DIR}/itkBSplineTransformSplineOrderTest.py
      ${ITK_TEST_OUTPUT_DIR}
  )
endif()
