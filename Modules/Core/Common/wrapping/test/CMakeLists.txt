# This test doesn't compile.  It exercises the bug I ran into if you multiply 2 vector images; if you
# try to compile it the compile fails.
# itk_add_test(NAME itkVectorMultiplyTest COMMAND ITKCommon2TestDriver itkVectorMultiplyTest)

itk_python_add_test(NAME itkMetaDataDictionaryPythonTest COMMAND itkMetaDataDictionaryTest.py)
itk_python_add_test(NAME itkDirectoryPythonTest COMMAND itkDirectoryTest.py)
itk_python_expression_add_test(NAME itkObjectPythonTest EXPRESSION "itkObject = itk.Object.New()")
itk_python_add_test(NAME itkIndexOffsetPythonTest COMMAND itkIndexOffsetTest.py)
itk_python_add_test(NAME itkImageDuplicatorPythonTest COMMAND itkImageDuplicatorTest.py)
execute_process(COMMAND ${Python3_EXECUTABLE} -c "import numpy"
  RESULT_VARIABLE _have_numpy_return_code
  OUTPUT_QUIET
  ERROR_QUIET
  )
if(_have_numpy_return_code EQUAL 0)
  itk_python_add_test(NAME itkImagePythonTest COMMAND itkImageTest.py)
  itk_python_add_test(NAME itkMatrixPythonTest COMMAND itkMatrixTest.py)
endif()
