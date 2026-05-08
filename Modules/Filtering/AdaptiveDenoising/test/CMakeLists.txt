itk_module_test()

set(AdaptiveDenoisingTests itkAdaptiveNonLocalMeansDenoisingImageFilterTest.cxx)

createtestdriver(AdaptiveDenoising "${AdaptiveDenoising-Test_LIBRARIES}" "${AdaptiveDenoisingTests}")

itk_add_test(
  NAME AdaptiveNonLocalMeansDenoisingImageFilterTest1
  COMMAND
    AdaptiveDenoisingTestDriver
    --compare
    DATA{Baseline/r16denoised_pearson_correlation.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/r16denoised_pearson_correlation.nrrd
    itkAdaptiveNonLocalMeansDenoisingImageFilterTest
    DATA{Input/r16slice.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/r16denoised_pearson_correlation.nrrd
    0
)

itk_add_test(
  NAME AdaptiveNonLocalMeansDenoisingImageFilterTest2
  COMMAND
    AdaptiveDenoisingTestDriver
    --compare
    DATA{Baseline/r16denoised_mean_squares.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/r16denoised_mean_squares.nrrd
    itkAdaptiveNonLocalMeansDenoisingImageFilterTest
    DATA{Input/r16slice.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/r16denoised_mean_squares.nrrd
    1
)
