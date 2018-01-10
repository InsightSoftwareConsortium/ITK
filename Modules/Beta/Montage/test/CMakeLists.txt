itk_module_test()

set(MontageTests
  itkPhaseCorrelationImageRegistrationMethodTest.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

#itk_add_test(NAME itkMinimalStandardRandomVariateGeneratorTest
  #COMMAND MontageTestDriver itkMinimalStandardRandomVariateGeneratorTest
  #)

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2cc
  COMMAND MontageTestDriver
  #--compare
    #${ITK_TEST_OUTPUT_DIR}/itkNormalDistributionImageSourceTestOutput.mha
    #DATA{Baseline/itkNormalDistributionImageSourceTestOutput.mha}
  itkPhaseCorrelationImageRegistrationMethodTest
    2cc
  )
