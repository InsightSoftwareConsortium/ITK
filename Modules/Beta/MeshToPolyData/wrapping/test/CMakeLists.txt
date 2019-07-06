itk_python_expression_add_test(NAME itkMeshToPolyDataFilterPythonTest
  EXPRESSION "filt = itk.MeshToPolyDataFilter.New()")
itk_python_expression_add_test(NAME itkPolyDataPythonTest
  EXPRESSION "poly_data = itk.PolyData.New()")
