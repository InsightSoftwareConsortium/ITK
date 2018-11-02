itk_module_test()

set(TotalVariationTests itkMyFilterTest.cxx)

createtestdriver(TotalVariation "${TotalVariation-Test_LIBRARIES}" "${TotalVariationTests}")

itk_add_test(
  NAME itkMyFilterTest
  COMMAND
    TotalVariationTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
    DATA{Baseline/itkMyFilterTestOutput.mha}
    itkMyFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkMyFilterTestOutput.mha
)
