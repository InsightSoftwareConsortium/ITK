if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkOBJMeshIOPythonTest
    EXPRESSION "io = itk.OBJMeshIO.New()")
endif()
