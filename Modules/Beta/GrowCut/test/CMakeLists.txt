itk_module_test()

set(GrowCutTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkFastGrowCutTest.cxx
  )

CreateTestDriver(GrowCut "${GrowCut-Test_LIBRARIES}" "${GrowCutTests}")

itk_add_test(NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND GrowCutTestDriver itkMinimalStandardRandomVariateGeneratorTest
  )

itk_add_test(NAME itkFastGrowCutTest
  COMMAND GrowCutTestDriver
    --compare
    DATA{Baseline/itkFastGrowCutTestOutput.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestOutput.mha
  itkFastGrowCutTest
    ${ITK_TEST_OUTPUT_DIR}/itkFastGrowCutTestOutput.mha
  )
