if(ITK_WRAP_PYTHON)
  itk_python_add_test(NAME itkTransformSerializationTest
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkTransformSerializationTest.py
    DATA{Baseline/BSplineDeformationTransformDisplacements5.txt}
    ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png
  )
endif()
