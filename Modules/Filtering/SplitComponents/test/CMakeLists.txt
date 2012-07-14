itk_module_test()
set(ITKSplitComponentsTests itkSplitComponentsImageFilterTest.cxx)
createtestdriver( ITKSplitComponents "${ITKSplitComponents-Test_LIBRARIES}" "${ITKSplitComponentsTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(
  NAME itkSplitComponentsImageFilterTest
  COMMAND
    ITKSplitComponentsTestDriver
    --compare
    DATA{Baseline/itkSplitComponentsImageFilterTestBaseline0.mha}
    itkSplitComponentsImageFilterTestOutput0.mha
    --compare
    DATA{Baseline/itkSplitComponentsImageFilterTestBaseline1.mha}
    itkSplitComponentsImageFilterTestOutput1.mha
    itkSplitComponentsImageFilterTest
    itkSplitComponentsImageFilterTestOutput
)
