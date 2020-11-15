# This test doesn't compile.  It exercises the bug I ran into if you multiply 2 vector images; if you
# try to compile it the compile fails.
# itk_add_test(NAME itkVectorMultiplyTest COMMAND ITKCommon2TestDriver itkVectorMultiplyTest)

itk_python_add_test(NAME itkMetaDataDictionaryPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkMetaDataDictionaryTest.py)
itk_python_add_test(NAME itkDirectoryPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkDirectoryTest.py)
itk_python_expression_add_test(NAME itkObjectPythonTest EXPRESSION "itkObject = itk.Object.New()")
itk_python_add_test(NAME itkIndexOffsetPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkIndexOffsetTest.py)
itk_python_add_test(NAME itkImageDuplicatorPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkImageDuplicatorTest.py)
execute_process(COMMAND ${Python3_EXECUTABLE} -c "import numpy"
  RESULT_VARIABLE _have_numpy_return_code
  OUTPUT_QUIET
  ERROR_QUIET
  )
if(_have_numpy_return_code EQUAL 0)
  itk_python_add_test(NAME itkImagePythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkImageTest.py)
  itk_python_add_test(NAME itkMatrixPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkMatrixTest.py)
endif()
