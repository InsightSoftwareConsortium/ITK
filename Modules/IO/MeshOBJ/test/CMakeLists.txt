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

itk_add_test(NAME itkMeshFileReadWriteOBJWithPointDataTest
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/bunny.obj}
      ${ITK_TEST_OUTPUT_DIR}/bunny.vtk
)
