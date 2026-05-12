itk_module_test()

set(
  HigherOrderAccurateGradientTests
  itkHigherOrderAccurateGradientImageFilterTest.cxx
  itkHigherOrderAccurateDerivativeImageFilterTest.cxx
)

createtestdriver(HigherOrderAccurateGradient "${HigherOrderAccurateGradient-Test_LIBRARIES}" "${HigherOrderAccurateGradientTests}")

itk_add_test(
  NAME itkHigherOrderAccurateGradientImageFilterTest
  COMMAND
    HigherOrderAccurateGradientTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateGradientImageFilterTest_Accuracy1_Magnitude.mha
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateGradientImageFilterTest_GradientImageFilter_Magnitude.mha
    itkHigherOrderAccurateGradientImageFilterTest
    DATA{Input/foot.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateGradientImageFilterTest
)

itk_add_test(
  NAME itkHigherOrderAccurateDerivativeImageFilterTest
  COMMAND
    HigherOrderAccurateGradientTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateDerivativeImageFilterTest_Accuracy1_Direction0.mha
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateDerivativeImageFilterTest_DerivativeImageFilter_Direction0.mha
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateDerivativeImageFilterTest_Accuracy1_Direction1.mha
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateDerivativeImageFilterTest_DerivativeImageFilter_Direction1.mha
    itkHigherOrderAccurateDerivativeImageFilterTest
    DATA{Input/foot.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkHigherOrderAccurateDerivativeImageFilterTest
)
