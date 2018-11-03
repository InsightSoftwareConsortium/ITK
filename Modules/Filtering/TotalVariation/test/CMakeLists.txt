itk_module_test()

set(TotalVariationTests itkProxTVTest.cxx)

createtestdriver(TotalVariation "${TotalVariation-Test_LIBRARIES}" "${TotalVariationTests}")

itk_add_test(
  NAME itkProxTVTest
  COMMAND
    TotalVariationTestDriver
    #   --compare
    #   ${ITK_TEST_OUTPUT_DIR}/itkProxTVTestOutput.mha
    #   DATA{Baseline/itkProxTVTestOutput.mha}
    itkProxTVTest
    ${ITK_TEST_OUTPUT_DIR}/itkProxTVTestOutput.mha
)
