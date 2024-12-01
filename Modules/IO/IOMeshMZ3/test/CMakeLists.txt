itk_module_test()

set(
  IOMeshMZ3Tests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkMZ3MeshIOTest.cxx
)

createtestdriver(IOMeshMZ3 "${IOMeshMZ3-Test_LIBRARIES}" "${IOMeshMZ3Tests}")

itk_add_test(
  NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND
    IOMeshMZ3TestDriver
    itkMinimalStandardRandomVariateGeneratorTest
)

itk_add_test(
  NAME itkMZ3MeshIOTest
  COMMAND
    IOMeshMZ3TestDriver
    --compare
    DATA{Baseline/itkMZ3MeshIOTestOutput.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutput.mha
    itkMZ3MeshIOTest
    ${ITK_TEST_OUTPUT_DIR}/itkMZ3MeshIOTestOutput.mha
)
