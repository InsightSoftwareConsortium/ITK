itk_module_test()

set(BoneEnhancementTests
  itkMaximumAbsoluteValueImageFilterTest.cxx
  itkMultiScaleHessianEnhancementImageFilterStaticMethodsTest.cxx
  itkHessianGaussianImageFilterTest.cxx
  )

CreateTestDriver(BoneEnhancement "${BoneEnhancement-Test_LIBRARIES}" "${BoneEnhancementTests}")

itk_add_test(NAME itkMaximumAbsoluteValueImageFilterTest
  COMMAND BoneEnhancementTestDriver itkMaximumAbsoluteValueImageFilterTest
  )

itk_add_test(NAME itkMultiScaleHessianEnhancementImageFilterStaticMethodsTest
  COMMAND BoneEnhancementTestDriver itkMultiScaleHessianEnhancementImageFilterStaticMethodsTest 
  )

itk_add_test(NAME itkHessianGaussianImageFilterTest
  COMMAND BoneEnhancementTestDriver itkHessianGaussianImageFilterTest 
  )


set(BoneEnhancementUnitTests
  itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest.cxx
  itkDescoteauxEigenToMeasureImageFilterUnitTest.cxx
  itkKrcahEigenToMeasureParameterEstimationFilterUnitTest.cxx
  )

CreateGoogleTestDriver(BoneEnhancementUnitTests "${BoneEnhancement-Test_LIBRARIES}" "${BoneEnhancementUnitTests}")
