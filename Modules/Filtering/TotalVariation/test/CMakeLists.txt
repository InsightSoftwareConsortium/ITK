itk_module_test()

set(TotalVariationTests itkProxTVImageFilterTest.cxx)

createtestdriver(TotalVariation "${TotalVariation-Test_LIBRARIES}" "${TotalVariationTests}")

itk_add_test(
  NAME itkProxTVImageFilterTest
  COMMAND
    TotalVariationTestDriver
    #   --compare
    #   ${ITK_TEST_OUTPUT_DIR}/itkProxTVImageFilterTestOutput.mha
    #   DATA{Baseline/itkProxTVImageFilterTestOutput.mha}
    itkProxTVImageFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkProxTVImageFilterTestOutput2D.mha
    ${ITK_TEST_OUTPUT_DIR}/itkProxTVImageFilterTestOutput3D.mha
)
