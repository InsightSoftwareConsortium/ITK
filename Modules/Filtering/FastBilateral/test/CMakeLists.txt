itk_module_test()

set(FastBilateralTests
  itkFastBilateralImageFilterTest.cxx
  )

CreateTestDriver(FastBilateral "${FastBilateral-Test_LIBRARIES}" "${FastBilateralTests}")

itk_add_test(NAME itkFastBilateralImageFilterTest
  COMMAND FastBilateralTestDriver
    --compare
    DATA{Baseline/itkFastBilateralImageFilterTestOutput.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkFastBilateralImageFilterTestOutput.mha
  itkFastBilateralImageFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkFastBilateralImageFilterTestOutput.mha
  )
