itk_module_test()

set(BoneEnhancementTests
  itkMaximumAbsoluteValueImageFilterTest.cxx
  itkHessianGaussianImageFilterTest.cxx
  )

CreateTestDriver(BoneEnhancement "${BoneEnhancement-Test_LIBRARIES}" "${BoneEnhancementTests}")

itk_add_test(NAME itkMaximumAbsoluteValueImageFilterTest
  COMMAND BoneEnhancementTestDriver itkMaximumAbsoluteValueImageFilterTest
  )

itk_add_test(NAME itkHessianGaussianImageFilterTest
  COMMAND BoneEnhancementTestDriver itkHessianGaussianImageFilterTest 
  )

set(BoneEnhancementUnitTests
  itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest.cxx
  itkDescoteauxEigenToMeasureImageFilterUnitTest.cxx
  itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest.cxx
  itkKrcahEigenToMeasureParameterEstimationFilterUnitTest.cxx
  )

CreateGoogleTestDriver(BoneEnhancementUnitTests "${BoneEnhancement-Test_LIBRARIES}" "${BoneEnhancementUnitTests}")
