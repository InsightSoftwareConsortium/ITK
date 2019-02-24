itk_module_test()

set(MeshToPolyDataTests
  itkMeshToPolyDataFilterTest.cxx
  )

CreateTestDriver(MeshToPolyData "${MeshToPolyData-Test_LIBRARIES}" "${MeshToPolyDataTests}")

itk_add_test(NAME itkMeshToPolyDataFilterTest
  COMMAND MeshToPolyDataTestDriver
  itkMeshToPolyDataFilterTest
    DATA{Input/cow.vtk}
  )
