itk_module_test()
set(ITK-FEMTests
itkFEMHeaderTest.cxx
)

CreateTestDriver(ITK-FEM  "${ITK-FEM-Test_LIBRARIES}" "${ITK-FEMTests}")

add_test(NAME itkFEMHeaderTest
      COMMAND ITK-FEMTestDriver itkFEMHeaderTest)
