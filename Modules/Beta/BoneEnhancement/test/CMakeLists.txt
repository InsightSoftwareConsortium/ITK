itk_module_test()

set(BoneEnhancementTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkNormalDistributionImageSourceTest.cxx
  itkMaximumAbsoluteValueImageFilterTest.cxx
  itkMultiScaleHessianEnhancementImageFilterStaticMethodsTest.cxx
  itkKrcahEigenToScalarParameterEstimationImageFilterTest.cxx
  itkKrcahEigenToScalarFunctorTest.cxx
  itkKrcahEigenToScalarFunctorImageFilterTest.cxx
  )

CreateTestDriver(BoneEnhancement "${BoneEnhancement-Test_LIBRARIES}" "${BoneEnhancementTests}")

itk_add_test(NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND BoneEnhancementTestDriver itkMinimalStandardRandomVariateGeneratorTest
  )

itk_add_test(NAME itkNormalDistributionImageSourceTest
  COMMAND BoneEnhancementTestDriver --without-threads
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
    DATA{Baseline/itkNormalDistributionImageSourceTestOutput.mha}
  itkNormalDistributionImageSourceTest
    ${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
  )

itk_add_test(NAME itkMaximumAbsoluteValueImageFilterTest
  COMMAND BoneEnhancementTestDriver itkMaximumAbsoluteValueImageFilterTest
  )

itk_add_test(NAME itkMultiScaleHessianEnhancementImageFilterStaticMethodsTest
  COMMAND BoneEnhancementTestDriver itkMultiScaleHessianEnhancementImageFilterStaticMethodsTest 
  )
  
itk_add_test(NAME itkKrcahEigenToScalarParameterEstimationImageFilterTest
  COMMAND BoneEnhancementTestDriver itkKrcahEigenToScalarParameterEstimationImageFilterTest 
  )

itk_add_test(NAME itkKrcahEigenToScalarFunctorTest
  COMMAND BoneEnhancementTestDriver itkKrcahEigenToScalarFunctorTest 
  )

itk_add_test(NAME itkKrcahEigenToScalarFunctorImageFilterTest
  COMMAND BoneEnhancementTestDriver itkKrcahEigenToScalarFunctorImageFilterTest 
  )
  