itk_module_test()

set(TotalVariationTests itkProxTVImageFilterTest.cxx)

set(Libraries ${TotalVariation-Test_LIBRARIES})
list(APPEND Libraries ${_proxTV_lib})

createtestdriver(TotalVariation "${Libraries}" "${TotalVariationTests}")

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
