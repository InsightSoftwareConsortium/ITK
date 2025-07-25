itk_module_test()
set(
  ITKPathTests
  itkPathIteratorTest.cxx
  itkChainCodePathTest.cxx
  itkChainCodePath2DTest.cxx
  itkHilbertPathTest.cxx
  itkOrthogonallyCorrected2DParametricPathTest.cxx
  itkPathToImageFilterTest.cxx
  itkPathToChainCodePathFilterTest.cxx
  itkFourierSeriesPathTest.cxx
  itkPathFunctionsTest.cxx
  itkPolyLineParametricPathTest.cxx
  itkChainCodeToFourierSeriesPathFilterTest.cxx
  itkExtractOrthogonalSwath2DImageFilterTest.cxx
  itkOrthogonalSwath2DPathFilterTest.cxx
  itkContourExtractor2DImageFilterTest.cxx
)

createtestdriver(ITKPath "${ITKPath-Test_LIBRARIES}" "${ITKPathTests}")

itk_add_test(
  NAME itkPathIteratorTest
  COMMAND
    ITKPathTestDriver
    itkPathIteratorTest
)
itk_add_test(
  NAME itkChainCodePathTest
  COMMAND
    ITKPathTestDriver
    itkChainCodePathTest
)
itk_add_test(
  NAME itkChainCodePath2DTest
  COMMAND
    ITKPathTestDriver
    itkChainCodePath2DTest
)
itk_add_test(
  NAME itkHilbertPathTest
  COMMAND
    ITKPathTestDriver
    itkHilbertPathTest
)
itk_add_test(
  NAME itkOrthogonallyCorrected2DParametricPathTest
  COMMAND
    ITKPathTestDriver
    itkOrthogonallyCorrected2DParametricPathTest
)
itk_add_test(
  NAME itkPathToImageFilterTest
  COMMAND
    ITKPathTestDriver
    itkPathToImageFilterTest
)
itk_add_test(
  NAME itkPathToChainCodePathFilterTest
  COMMAND
    ITKPathTestDriver
    itkPathToChainCodePathFilterTest
)
itk_add_test(
  NAME itkFourierSeriesPathTest
  COMMAND
    ITKPathTestDriver
    itkFourierSeriesPathTest
)
itk_add_test(
  NAME itkPathFunctionsTest
  COMMAND
    ITKPathTestDriver
    itkPathFunctionsTest
)
itk_add_test(
  NAME itkPolyLineParametricPathTest
  COMMAND
    ITKPathTestDriver
    itkPolyLineParametricPathTest
)
itk_add_test(
  NAME itkChainCodeToFourierSeriesPathFilterTest
  COMMAND
    ITKPathTestDriver
    itkChainCodeToFourierSeriesPathFilterTest
)
itk_add_test(
  NAME itkExtractOrthogonalSwath2DImageFilterTest
  COMMAND
    ITKPathTestDriver
    --compare
    DATA{Baseline/ExtractOrthogonalSwath2DImageFilterTest.png}
    ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
    itkExtractOrthogonalSwath2DImageFilterTest
    ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
)
itk_add_test(
  NAME itkOrthogonalSwath2DPathFilterTest
  COMMAND
    ITKPathTestDriver
    itkOrthogonalSwath2DPathFilterTest
)
itk_add_test(
  NAME itkContourExtractor2DImageFilterTest
  COMMAND
    ITKPathTestDriver
    itkContourExtractor2DImageFilterTest
    DATA{Input/ContourExtractor2DTest.tif}
)
