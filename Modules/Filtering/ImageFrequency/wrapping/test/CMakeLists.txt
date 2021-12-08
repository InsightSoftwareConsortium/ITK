if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkUnaryFrequencyDomainFilterPythonTest
    EXPRESSION "instance = itk.UnaryFrequencyDomainFilter.New()")

  itk_python_expression_add_test(NAME itkFrequencyBandImageFilterPythonTest
    EXPRESSION "instance = itk.FrequencyBandImageFilter.New()")
endif()
