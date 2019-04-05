# This test doesn't compile.  It exercises the bug I ran into if you multiply 2 vector images; if you
# try to compile it the compile fails.
# itk_add_test(NAME itkVectorMultiplyTest COMMAND ITKCommon2TestDriver itkVectorMultiplyTest)

itk_python_add_test(NAME itkMetaDataDictionaryPythonTest COMMAND itkMetaDataDictionaryTest.py)
itk_python_add_test(NAME itkDirectoryPythonTest COMMAND itkDirectoryTest.py)
itk_python_expression_add_test(NAME itkObjectPythonTest EXPRESSION "itkObject = itk.Object.New()")
itk_python_add_test(NAME itkIndexOffsetPythonTest COMMAND itkIndexOffsetTest.py)
