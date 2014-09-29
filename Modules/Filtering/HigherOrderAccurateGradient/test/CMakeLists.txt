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
    itkHigherOrderAccurateDerivativeImageFilterTest_Accuracy1_Direction0.mha
    itkHigherOrderAccurateDerivativeImageFilterTest_DerivativeImageFilter_Direction0.mha
    --compare
    itkHigherOrderAccurateDerivativeImageFilterTest_Accuracy1_Direction1.mha
    itkHigherOrderAccurateDerivativeImageFilterTest_DerivativeImageFilter_Direction1.mha
    itkHigherOrderAccurateGradientImageFilterTest
    DATA{Input/foot.mha}
    itkHigherOrderAccurateGradientImageFilterTest
)

itk_add_test(
  NAME itkHigherOrderAccurateDerivativeImageFilterTest
  COMMAND
    HigherOrderAccurateGradientTestDriver
    --compare
    itkHigherOrderAccurateGradientImageFilterTest_Accuracy1_Magnitude.mha
    itkHigherOrderAccurateGradientImageFilterTest_GradientImageFilter_Magnitude.mha
    itkHigherOrderAccurateDerivativeImageFilterTest
    DATA{Input/foot.mha}
    itkHigherOrderAccurateDerivativeImageFilterTest
)
