if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterPythonTest
    EXPRESSION "instance = itk.DiffeomorphicDemonsRegistrationFilter.New()")
endif()
