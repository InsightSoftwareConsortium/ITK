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
)
