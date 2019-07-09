itk_python_expression_add_test(NAME itkMeshToPolyDataFilterPythonTest
  EXPRESSION "filt = itk.MeshToPolyDataFilter.New()")
itk_python_add_test(NAME itkMeshToPolyDataFilterPythonTest2
  COMMAND itkMeshToPolyDataFilterTest2.py)
itk_python_expression_add_test(NAME itkPolyDataPythonTest
  EXPRESSION "poly_data = itk.PolyData.New()")

execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import numpy"
  RESULT_VARIABLE _have_numpy_return_code
  OUTPUT_QUIET
  ERROR_QUIET
  )
if(_have_numpy_return_code EQUAL 0)
  itk_python_add_test(NAME itkPyVectorContainerTest
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPyVectorContainerTest.py
  )
endif()
