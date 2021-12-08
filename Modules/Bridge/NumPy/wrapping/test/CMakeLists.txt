if(ITK_WRAP_PYTHON)
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
  itk_python_add_test(NAME itkImageMetaDataSetGetItem.py
                        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkImageMetaDataSetGetItem.py
                        DATA{${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm}
  )
  itk_python_add_test(NAME itkPyVectorContainerTest
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPyVectorContainerTest.py
  )

  if(NOT WIN32)
    itk_python_add_test(NAME itkPyBufferMemoryLeakTest
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPyBufferMemoryLeakTest.py
    )
  endif()
endif()
