itk_module_test()

set(
  BSplineGradientTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkNormalDistributionImageSourceTest.cxx
  itkMyFilterTest.cxx
)

createtestdriver(BSplineGradient "${BSplineGradient-Test_LIBRARIES}" "${BSplineGradientTests}")

itk_add_test(
  NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND
    BSplineGradientTestDriver
    itkMinimalStandardRandomVariateGeneratorTest
)

itk_add_test(
  NAME itkMyFilterTest
  COMMAND
    BSplineGradientTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
    DATA{Baseline/itkMyFilterTestOutput.mha}
    itkMyFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
)

itk_add_test(
  NAME itkNormalDistributionImageSourceTest
  COMMAND
    BSplineGradientTestDriver
    --without-threads
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
    DATA{Baseline/itkNormalDistributionImageSourceTestOutput.mha}
    itkNormalDistributionImageSourceTest
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
)
