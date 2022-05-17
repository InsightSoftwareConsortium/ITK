itk_module_test()

set(
  IOMeshSWCTests
  itkMinimalStandardRandomVariateGeneratorTest.cxx
  itkMyFilterTest.cxx
)

createtestdriver(IOMeshSWC "${IOMeshSWC-Test_LIBRARIES}" "${IOMeshSWCTests}")

itk_add_test(
  NAME itkMinimalStandardRandomVariateGeneratorTest
  COMMAND
    IOMeshSWCTestDriver
    itkMinimalStandardRandomVariateGeneratorTest
)

itk_add_test(
  NAME itkMyFilterTest
  COMMAND
    IOMeshSWCTestDriver
    --compare
    DATA{Baseline/itkMyFilterTestOutput.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
    itkMyFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
)
