itk_module_test()

set(
  MeshToPolyDataTests
  itkImageToPointSetFilterTest.cxx
  itkMeshToPolyDataFilterTest.cxx
  itkMeshToPolyDataFilterCellDataTest.cxx
  itkMeshToPolyDataFilterLinePolyLineCellDataTest.cxx
  itkPolyDataTest.cxx
  itkPolyDataToMeshFilterTest.cxx
)

createtestdriver(MeshToPolyData "${MeshToPolyData-Test_LIBRARIES}" "${MeshToPolyDataTests}")

itk_add_test(
  NAME itkMeshToPolyDataFilterTest
  COMMAND
    MeshToPolyDataTestDriver
    itkMeshToPolyDataFilterTest
    DATA{Input/cow.vtk}
    ${ITK_TEST_OUTPUT_DIR}/itkMeshToPolyDataFilterTest.vtk
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkMeshToPolyDataFilterTest.vtk
)

itk_add_test(
  NAME itkMeshToPolyDataFilterCellDataTest
  COMMAND
    MeshToPolyDataTestDriver
    itkMeshToPolyDataFilterCellDataTest
)

itk_add_test(
  NAME itkMeshToPolyDataFilterLinePolyLineCellDataTest
  COMMAND
    MeshToPolyDataTestDriver
    itkMeshToPolyDataFilterLinePolyLineCellDataTest
)

itk_add_test(
  NAME itkPolyDataTest
  COMMAND
    MeshToPolyDataTestDriver
    itkPolyDataTest
)

itk_add_test(
  NAME itkImageToPointSetFilterTest
  COMMAND
    MeshToPolyDataTestDriver
    itkImageToPointSetFilterTest
    DATA{Input/foot.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkImageToPointSetFilterTestOutput.vtk
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkImageToPointSetFilterTestOutput.vtk
)

itk_add_test(
  NAME itkPolyDataToMeshFilterTest
  COMMAND
    MeshToPolyDataTestDriver
    itkPolyDataToMeshFilterTest
)
