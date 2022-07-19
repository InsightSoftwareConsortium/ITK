itk_module_test()

set(FpfhTests itkPointFeatureTest.cxx)

createtestdriver(Fpfh "${Fpfh-Test_LIBRARIES}" "${FpfhTests}")

itk_add_test(
  NAME itkPointFeatureTest
  COMMAND
    FpfhTestDriver
    itkPointFeatureTest
    ${ITK_TEST_OUTPUT_DIR}/itkPointFeatureTestOutput.mha
)
