itk_module_test()

set(ITKIOMeshOBJTests
  itkMeshFileReadWriteTest.cxx
  itkOBJMeshIOTest.cxx
)

CreateTestDriver(ITKIOMeshOBJ "${ITKIOMeshOBJ-Test_LIBRARIES}" "${ITKIOMeshOBJTests}")

itk_add_test(NAME itkMeshFileReadWriteOBJTest
      COMMAND ITKIOMeshOBJTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/box.obj}
      ${ITK_TEST_OUTPUT_DIR}/box.obj
)

itk_add_test(NAME itkMeshFileReadWriteOBJWithPointDataTest
      COMMAND ITKIOMeshOBJTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/bunny.obj}
      ${ITK_TEST_OUTPUT_DIR}/bunny.vtk
)

itk_add_test(NAME itkOBJMeshIOTest1
      COMMAND ITKIOMeshOBJTestDriver itkOBJMeshIOTest
      DATA{Baseline/box.obj}
      ${ITK_TEST_OUTPUT_DIR}/objmeshiobox.obj
      DATA{${ITK_DATA_ROOT}/Input/tetrahedron.vtk}
      ${ITK_TEST_OUTPUT_DIR}/obj2vtkbox.vtk
      0 1 1 1 1 8 0 6 0
)

itk_add_test(NAME itkOBJMeshIOTest2
      COMMAND ITKIOMeshOBJTestDriver itkOBJMeshIOTest
      DATA{Baseline/bunny.obj}
      ${ITK_TEST_OUTPUT_DIR}/objmeshiobunny.obj
      DATA{${ITK_DATA_ROOT}/Input/tetrahedron.vtk}
      ${ITK_TEST_OUTPUT_DIR}/obj2vtkbunny.vtk
      0 1 1 1 1 2503 4968 4968 0
)
