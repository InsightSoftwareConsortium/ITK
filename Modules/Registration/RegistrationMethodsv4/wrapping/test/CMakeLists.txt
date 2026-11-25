if(ITK_WRAP_PYTHON)
  foreach(d 2 3)
    list(FIND ITK_WRAP_IMAGE_DIMS ${d} wrap_dimension_index)
    if(
      ITKBridgeNumPy_ENABLED
      AND
        ITK_WRAP_float
      AND
        wrap_dimension_index
          GREATER
          -1
    )
      itk_python_add_test(
        NAME itkSyNImageRegistrationMethodMultiresolution${d}DPythonTest
        COMMAND
          ${CMAKE_CURRENT_SOURCE_DIR}/itkSyNImageRegistrationMethodTest.py
          ${d}
      )
    endif()
  endforeach()

  itk_python_expression_add_test(
    NAME itkBSplineSyNImageRegistrationMethodPythonTest
    EXPRESSION "instance = itk.BSplineSyNImageRegistrationMethod.New()"
  )

  itk_python_expression_add_test(
    NAME itkImageRegistrationMethodv4PythonTest
    EXPRESSION "instance = itk.ImageRegistrationMethodv4.New()"
  )

  itk_python_expression_add_test(
    NAME itkSyNImageRegistrationMethodPythonTest
    EXPRESSION "instance = itk.SyNImageRegistrationMethod.New()"
  )

  itk_python_expression_add_test(
    NAME itkTimeVaryingBSplineVelocityFieldImageRegistrationMethodv4PythonTest
    EXPRESSION
      "instance = itk.TimeVaryingBSplineVelocityFieldImageRegistrationMethod.New()"
  )

  itk_python_expression_add_test(
    NAME itkTimeVaryingVelocityFieldImageRegistrationMethodv4PythonTest
    EXPRESSION
      "instance = itk.TimeVaryingVelocityFieldImageRegistrationMethodv4.New()"
  )
endif()
