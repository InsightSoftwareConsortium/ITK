if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkNormalVariateGeneratorPythonTest
    EXPRESSION "itkNormalVariateGenerator = itk.NormalVariateGenerator.New()")
endif()
