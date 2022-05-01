itk_module_test()

set(ITKIOMeshOFFTests
  itkMeshFileReadWriteTest.cxx
  itkOFFMeshIOTest.cxx
)

CreateTestDriver(ITKIOMeshOFF "${ITKIOMeshOFF-Test_LIBRARIES}" "${ITKIOMeshOFFTests}")

itk_add_test(NAME itkMeshFileReadWriteOFFTest
      COMMAND ITKIOMeshOFFTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/octa.off}
      ${ITK_TEST_OUTPUT_DIR}/octa.off
)
itk_add_test(NAME itkOFFMeshIOTest
      COMMAND ITKIOMeshOFFTestDriver itkOFFMeshIOTest
      DATA{Baseline/octa.off}
      ${ITK_TEST_OUTPUT_DIR}/offmeshioocta.off
      DATA{${ITK_DATA_ROOT}/Input/tetrahedron.vtk}
      ${ITK_TEST_OUTPUT_DIR}/off2vtkocta.vtk
      0 1 1 1 1 6 0 8 0
)
