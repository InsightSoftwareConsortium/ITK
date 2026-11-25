itk_module_test()

set(
  ITKIOMeshBYUTests
  itkBYUMeshIOTest.cxx
  itkMeshFileReadWriteTest.cxx
)

createtestdriver(ITKIOMeshBYU "${ITKIOMeshBYU-Test_LIBRARIES}" "${ITKIOMeshBYUTests}")

itk_add_test(
  NAME itkBYUMeshIOTest
  COMMAND
    ITKIOMeshBYUTestDriver
    itkBYUMeshIOTest
    DATA{Baseline/cube.byu}
    ${ITK_TEST_OUTPUT_DIR}/byumeshiocube.byu
    DATA{${ITK_DATA_ROOT}/Input/tetrahedron.vtk}
    ${ITK_TEST_OUTPUT_DIR}/byu2vtkcube.vtk
    0
    1
    1
    1
    1
    8
    0
    6
    0
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/byumeshiocube.byu
    ${ITK_TEST_OUTPUT_DIR}/byu2vtkcube.vtk
)
itk_add_test(
  NAME itkMeshFileReadWriteTest06
  COMMAND
    ITKIOMeshBYUTestDriver
    itkMeshFileReadWriteTest
    DATA{Baseline/cube.byu}
    ${ITK_TEST_OUTPUT_DIR}/cube.byu
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/cube.byu
)
