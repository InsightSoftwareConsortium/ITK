itk_module_test()

set(MontageTests
  )

#CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

#itk_add_test(NAME itkMinimalStandardRandomVariateGeneratorTest
  #COMMAND MontageTestDriver itkMinimalStandardRandomVariateGeneratorTest
  #)

#itk_add_test(NAME itkNormalDistributionImageSourceTest
  #COMMAND MontageTestDriver --without-threads
  #--compare
    #${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
    #DATA{Baseline/itkNormalDistributionImageSourceTestOutput.mha}
  #itkNormalDistributionImageSourceTest
    #${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
  #)
