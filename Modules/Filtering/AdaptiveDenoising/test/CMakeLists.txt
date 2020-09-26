itk_module_test()

set(
  AdaptiveDenoisingTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkAdaptiveNonLocalMeansDenoisingImageFilterTest.cxx
)

createtestdriver(AdaptiveDenoising "${AdaptiveDenoising-Test_LIBRARIES}" "${AdaptiveDenoisingTests}")

itk_add_test(
  NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND
    AdaptiveDenoisingTestDriver
    itkMinimalStandardRandomVariateGeneratorTest
)

itk_add_test(
  NAME itkAdaptiveNonLocalMeansDenoisingImageFilterTest
  COMMAND
    AdaptiveDenoisingTestDriver
    --compare
    DATA{Baseline/itkAdaptiveNonLocalMeansDenoisingImageFilterTestOutput.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkAdaptiveNonLocalMeansDenoisingImageFilterTestOutput.mha
    itkAdaptiveNonLocalMeansDenoisingImageFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkAdaptiveNonLocalMeansDenoisingImageFilterTestOutput.mha
)
