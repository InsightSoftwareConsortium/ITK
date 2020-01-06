execute_process(COMMAND ${Python3_EXECUTABLE} -c "import numpy"
  RESULT_VARIABLE _have_numpy_return_code
  OUTPUT_QUIET
  ERROR_QUIET
  )
if(_have_numpy_return_code EQUAL 0)
  itk_python_add_test(NAME itkPyBufferTest
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPyBufferTest.py
  )
  itk_python_add_test(NAME itkPyVnlTest
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPyVnlTest.py
  )
  itk_python_add_test(NAME itkPyBufferPipelineTest.py
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPyBufferPipelineTest.py
                        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  )
endif()

if(NOT WIN32)
  itk_python_add_test(NAME itkPyBufferMemoryLeakTest
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPyBufferMemoryLeakTest.py
  )
endif()
