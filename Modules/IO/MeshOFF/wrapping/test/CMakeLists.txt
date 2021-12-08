if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkOFFMeshIOPythonTest
    EXPRESSION "io = itk.OFFMeshIO.New()")
endif()
