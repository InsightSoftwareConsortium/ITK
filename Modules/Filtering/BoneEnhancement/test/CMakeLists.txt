itk_module_test()

set(BoneEnhancementTests itkHessianGaussianImageFilterTest.cxx)

createtestdriver(BoneEnhancement "${BoneEnhancement-Test_LIBRARIES}" "${BoneEnhancementTests}")

itk_add_test(
  NAME itkHessianGaussianImageFilterTest
  COMMAND
    BoneEnhancementTestDriver
    itkHessianGaussianImageFilterTest
)

set(
  BoneEnhancementUnitTests
  itkMaximumAbsoluteValueImageFilterUnitTest.cxx
  itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest.cxx
  itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest.cxx
  itkDescoteauxEigenToMeasureImageFilterUnitTest.cxx
  itkKrcahEigenToMeasureParameterEstimationFilterUnitTest.cxx
)

creategoogletestdriver(BoneEnhancementUnitTests "${BoneEnhancement-Test_LIBRARIES}" "${BoneEnhancementUnitTests}")
