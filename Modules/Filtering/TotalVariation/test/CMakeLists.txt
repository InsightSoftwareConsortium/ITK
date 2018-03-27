itk_module_test()

set(
  TotalVariationTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkNormalDistributionImageSourceTest.cxx
)

createtestdriver(TotalVariation "${TotalVariation-Test_LIBRARIES}" "${TotalVariationTests}")

itk_add_test(
  NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND
    TotalVariationTestDriver
    itkMinimalStandardRandomVariateGeneratorTest
)

itk_add_test(
  NAME itkNormalDistributionImageSourceTest
  COMMAND
    TotalVariationTestDriver
    --without-threads
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
    DATA{Baseline/itkNormalDistributionImageSourceTestOutput.mha}
    itkNormalDistributionImageSourceTest
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
)
