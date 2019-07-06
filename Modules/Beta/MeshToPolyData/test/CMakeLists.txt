itk_module_test()

set(MeshToPolyDataTests
  itkMeshToPolyDataFilterTest.cxx
  itkPolyDataTest.cxx
  )

CreateTestDriver(MeshToPolyData "${MeshToPolyData-Test_LIBRARIES}" "${MeshToPolyDataTests}")

itk_add_test(NAME itkMeshToPolyDataFilterTest
  COMMAND MeshToPolyDataTestDriver
  itkMeshToPolyDataFilterTest
    DATA{Input/cow.vtk}
    ${ITK_TEST_OUTPUT_DIR}/itkMeshToPolyDataFilterTest.vtk
  )

itk_add_test(NAME itkPolyDataTest
  COMMAND MeshToPolyDataTestDriver
  itkPolyDataTest
  )
