if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkBSplineSyNImageRegistrationMethodPythonTest
    EXPRESSION "instance = itk.BSplineSyNImageRegistrationMethod.New()")

  itk_python_expression_add_test(NAME itkImageRegistrationMethodv4PythonTest
    EXPRESSION "instance = itk.ImageRegistrationMethodv4.New()")

  itk_python_expression_add_test(NAME itkSyNImageRegistrationMethodPythonTest
    EXPRESSION "instance = itk.SyNImageRegistrationMethod.New()")

  itk_python_expression_add_test(NAME itkTimeVaryingBSplineVelocityFieldImageRegistrationMethodv4PythonTest
    EXPRESSION "instance = itk.TimeVaryingBSplineVelocityFieldImageRegistrationMethod.New()")

  itk_python_expression_add_test(NAME itkTimeVaryingVelocityFieldImageRegistrationMethodv4PythonTest
    EXPRESSION "instance = itk.TimeVaryingVelocityFieldImageRegistrationMethodv4.New()")
endif()
