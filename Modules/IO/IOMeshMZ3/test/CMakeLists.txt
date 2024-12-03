itk_module_test()

set(IOMeshMZ3Tests itkMZ3MeshIOTest.cxx)

createtestdriver(IOMeshMZ3 "${IOMeshMZ3-Test_LIBRARIES}" "${IOMeshMZ3Tests}")

itk_add_test(
  NAME itkMZ3MeshIOTest1
  COMMAND
    IOMeshMZ3TestDriver
    itkMZ3MeshIOTest
    DATA{Input/11ScalarMesh.mz3}
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutput1.mz3
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutputCompressed1.mz3
)

itk_add_test(
  NAME itkMZ3MeshIOTest2
  COMMAND
    IOMeshMZ3TestDriver
    itkMZ3MeshIOTest
    DATA{Input/3Mesh.mz3}
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutput2.mz3
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutputCompressed2.mz3
)

itk_add_test(
  NAME itkMZ3MeshIOTest3
  COMMAND
    IOMeshMZ3TestDriver
    itkMZ3MeshIOTest
    DATA{Input/BrainMesh_ICBM152.lh.motor.mz3}
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutput3.mz3
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutputCompressed3.mz3
)
