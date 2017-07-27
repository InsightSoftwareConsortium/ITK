itk_module_test()

set(BoneMorphometryTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkNormalDistributionImageSourceTest.cxx
  )

CreateTestDriver(BoneMorphometry "${BoneMorphometry-Test_LIBRARIES}" "${BoneMorphometryTests}")

itk_add_test(NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND BoneMorphometryTestDriver itkMinimalStandardRandomVariateGeneratorTest
  )

itk_add_test(NAME itkNormalDistributionImageSourceTest
  COMMAND BoneMorphometryTestDriver --without-threads
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
    DATA{Baseline/itkNormalDistributionImageSourceTestOutput.mha}
  itkNormalDistributionImageSourceTest
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
  )
