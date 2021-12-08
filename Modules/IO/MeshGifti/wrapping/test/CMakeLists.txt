if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkGiftiMeshIOPythonTest
    EXPRESSION "io = itk.GiftiMeshIO.New()")
endif()
