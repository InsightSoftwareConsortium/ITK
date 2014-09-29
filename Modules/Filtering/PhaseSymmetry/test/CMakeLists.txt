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
    itkButterworthFilterFreqImageSourceTest
    ${ITK_TEST_OUTPUT_DIR}/itkButterworthFilterFreqImageSourceTest.mha
)

itk_add_test(
  NAME itkLogGaborFreqImageSourceTest
  COMMAND
    PhaseSymmetryTestDriver
    itkLogGaborFreqImageSourceTest
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
    itkSinusoidImageSourceTest
)
