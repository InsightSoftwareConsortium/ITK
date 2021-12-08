if(ITK_WRAP_PYTHON)
  itk_python_add_test(NAME itkGPUImagePythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkGPUImageTest.py)
  itk_python_add_test(NAME itkGPUReductionPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkGPUReductionTest.py)
endif()
