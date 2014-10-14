itk_module_test()

set(
  PhaseSymmetryTests
  itkPhaseSymmetryImageFilterTest.cxx
  itkButterworthFilterFreqImageSourceTest.cxx
  itkLogGaborFreqImageSourceTest.cxx
  itkSteerableFilterFreqImageSourceTest.cxx
  itkSinusoidImageSourceTest.cxx
)

createtestdriver( PhaseSymmetry "${PhaseSymmetry-Test_LIBRARIES}" "${PhaseSymmetryTests}")

itk_add_test(
  NAME itkPhaseSymmetryImageFilterTest
  COMMAND
    PhaseSymmetryTestDriver
    itkPhaseSymmetryImageFilterTest
    DATA{Input/HeartUltrasound.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkPhaseSymmetryImageFilterTest.mha
)

itk_add_test(
  NAME itkButterworthFilterFreqImageSourceTest
  COMMAND
    PhaseSymmetryTestDriver
    --compare
    DATA{Baseline/itkButterworthFilterFreqImageSourceTestFilter.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkButterworthFilterFreqImageSourceTestFilter.mha
    itkButterworthFilterFreqImageSourceTest
    DATA{Input/SinusoidImage.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkButterworthFilterFreqImageSourceTestFilter.mha
    ${ITK_TEST_OUTPUT_DIR}/itkButterworthFilterFreqImageSourceTestOutput.mha
    ${ITK_TEST_OUTPUT_DIR}/itkButterworthFilterFreqImageSourceTestInputFFT.mha
    ${ITK_TEST_OUTPUT_DIR}/itkButterworthFilterFreqImageSourceTestOutputFFT.mha
)

itk_add_test(
  NAME itkLogGaborFreqImageSourceTest
  COMMAND
    PhaseSymmetryTestDriver
    --compare
    DATA{Baseline/itkLogGaborFreqImageSourceTestFilter.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkLogGaborFreqImageSourceTestFilter.mha
    itkLogGaborFreqImageSourceTest
    DATA{Input/SinusoidImage.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkLogGaborFreqImageSourceTestFilter.mha
    ${ITK_TEST_OUTPUT_DIR}/itkLogGaborFreqImageSourceTestOutput.mha
    ${ITK_TEST_OUTPUT_DIR}/itkLogGaborFreqImageSourceTestInputFFT.mha
    ${ITK_TEST_OUTPUT_DIR}/itkLogGaborFreqImageSourceTestOutputFFT.mha
)

itk_add_test(
  NAME itkSteerableFilterFreqImageSourceTest
  COMMAND
    PhaseSymmetryTestDriver
    itkSteerableFilterFreqImageSourceTest
)

itk_add_test(
  NAME itkSinusoidImageSourceTest
  COMMAND
    PhaseSymmetryTestDriver
    --compare
    DATA{Baseline/itkSinusoidImageSourceTest.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkSinusoidImageSourceTest.mha
    itkSinusoidImageSourceTest
    ${ITK_TEST_OUTPUT_DIR}/itkSinusoidImageSourceTest.mha
    0.02
    0.1
    0.2
    0.2
)
