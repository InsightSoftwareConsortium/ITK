itk_module_test()

set(MeshToPolyDataTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkNormalDistributionImageSourceTest.cxx
  itkMeshToPolyDataFilterTest.cxx
  )

CreateTestDriver(MeshToPolyData "${MeshToPolyData-Test_LIBRARIES}" "${MeshToPolyDataTests}")

itk_add_test(NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND MeshToPolyDataTestDriver itkMinimalStandardRandomVariateGeneratorTest
  )

itk_add_test(NAME itkMeshToPolyDataFilterTest
  COMMAND MeshToPolyDataTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkMeshToPolyDataFilterTestOutput.mha
    DATA{Baseline/itkMeshToPolyDataFilterTestOutput.mha}
  itkMeshToPolyDataFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkMeshToPolyDataFilterTestOutput.mha
  )

itk_add_test(NAME itkNormalDistributionImageSourceTest
  COMMAND MeshToPolyDataTestDriver --without-threads
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
    DATA{Baseline/itkNormalDistributionImageSourceTestOutput.mha}
  itkNormalDistributionImageSourceTest
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
  )
