itk_module_test()

set(ITKIOMeshOBJTests
  itkMeshFileReadWriteTest.cxx
)

CreateTestDriver(ITKIOMeshOBJ "${ITKIOMeshOBJ-Test_LIBRARIES}" "${ITKIOMeshOBJTests}" )

itk_add_test(NAME itkMeshFileReadWriteOBJTest
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/box.obj}
      ${ITK_TEST_OUTPUT_DIR}/box.obj
)

itk_python_expression_add_test(NAME itkOBJMeshIOPythonTest
  EXPRESSION "io = itk.OBJMeshIO.New()")
