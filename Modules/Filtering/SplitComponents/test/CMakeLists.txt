itk_module_test()
set(SplitComponentsTests itkSplitComponentsImageFilterTest.cxx)
createtestdriver( SplitComponents "${SplitComponents-Test_LIBRARIES}" "${SplitComponentsTests}")

itk_add_test(
  NAME itkSplitComponentsImageFilterTest
  COMMAND
    SplitComponentsTestDriver
    --compare
    DATA{Baseline/itkSplitComponentsImageFilterTestBaseline0.mha}
    itkSplitComponentsImageFilterTestOutput0.mha
    --compare
    DATA{Baseline/itkSplitComponentsImageFilterTestBaseline1.mha}
    itkSplitComponentsImageFilterTestOutput1.mha
    itkSplitComponentsImageFilterTest
    itkSplitComponentsImageFilterTestOutput
)
