itk_module_test()

set(AdaptiveDenoisingTests itkAdaptiveNonLocalMeansDenoisingImageFilterTest.cxx)

createtestdriver(AdaptiveDenoising "${AdaptiveDenoising-Test_LIBRARIES}" "${AdaptiveDenoisingTests}")

itk_add_test(
  NAME AdaptiveNonLocalMeansDenoisingImageFilterTest1
  COMMAND
    AdaptiveDenoisingTestDriver
    --compare
    DATA{Baseline/r16denoised.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/r16denoised.nrrd
    itkAdaptiveNonLocalMeansDenoisingImageFilterTest
    DATA{Input/r16slice.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/r16denoised.nrrd
)
