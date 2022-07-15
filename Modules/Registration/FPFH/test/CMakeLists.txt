itk_module_test()

set(FpfhTests itkMyFilterTest.cxx)

createtestdriver(Fpfh "${Fpfh-Test_LIBRARIES}" "${FpfhTests}")

itk_add_test(
  NAME itkMyFilterTest
  COMMAND
    FpfhTestDriver
    --compare
    DATA{Baseline/itkMyFilterTestOutput.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
    itkMyFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
)
