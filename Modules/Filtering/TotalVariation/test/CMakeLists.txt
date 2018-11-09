itk_module_test()

set(TotalVariationTests itkProxTVTest.cxx)

set(Libraries ${TotalVariation-Test_LIBRARIES})
list(APPEND Libraries ${_proxTV_lib})

createtestdriver(TotalVariation "${Libraries}" "${TotalVariationTests}")

itk_add_test(
  NAME itkProxTVTest
  COMMAND
    TotalVariationTestDriver
    #   --compare
    #   ${ITK_TEST_OUTPUT_DIR}/itkProxTVTestOutput.mha
    #   DATA{Baseline/itkProxTVTestOutput.mha}
    itkProxTVTest
    ${ITK_TEST_OUTPUT_DIR}/itkProxTVTestOutput2D.mha
    ${ITK_TEST_OUTPUT_DIR}/itkProxTVTestOutput3D.mha
)
