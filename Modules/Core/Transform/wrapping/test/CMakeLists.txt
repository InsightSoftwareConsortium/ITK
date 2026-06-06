if(ITK_WRAP_PYTHON)
  itk_python_add_test(
    NAME itkTransformSerializationTest
    COMMAND
      ${CMAKE_CURRENT_SOURCE_DIR}/itkTransformSerializationTest.py
      DATA{Baseline/BSplineDeformationTransformDisplacements5.txt}
      ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png
  )

  itk_python_expression_add_test(
    NAME PythonInstantiateBSplineTransform2D2D
    EXPRESSION "t = itk.BSplineTransform[itk.D, 2, 2].New()"
  )

  list(FIND ITK_WRAP_IMAGE_DIMS 4 wrap_4_index)
  if(wrap_4_index GREATER -1)
    itk_python_expression_add_test(
      NAME PythonInstantiateBSplineTransform4D4D
      EXPRESSION "t = itk.BSplineTransform[itk.D, 4, 4].New()"
    )
  endif()
endif()
