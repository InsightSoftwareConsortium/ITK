itk_module_test()

set(FastBilateralTests
  itkFastBilateralImageFilterTest.cxx
  itkFastBilateralImageFilterTest2.cxx
  itkFastBilateralImageFilterTest3.cxx
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

itk_add_test(NAME itkFastBilateralImageFilterTest2
  COMMAND FastBilateralTestDriver
    --compare
    DATA{Baseline/itkFastBilateralImageFilterTest2Output.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFastBilateralImageFilterTest2Output.png
  itkFastBilateralImageFilterTest2
    DATA{Input/cake_easy.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFastBilateralImageFilterTest2Output.png
  )

itk_add_test(NAME itkFastBilateralImageFilterTest3
  COMMAND FastBilateralTestDriver
    --compare
    DATA{Baseline/itkFastBilateralImageFilterTest3Output.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFastBilateralImageFilterTest3Output.png
  itkFastBilateralImageFilterTest3
    DATA{Input/cake_easy.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFastBilateralImageFilterTest3Output.png
  )
