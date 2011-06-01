itk_module_test()
set(ITK-PathTests
itkPathIteratorTest.cxx
itkPathHeaderTest.cxx
itkChainCodePathTest.cxx
itkChainCodePath2DTest.cxx
itkOrthogonallyCorrected2DParametricPathTest.cxx
itkPathToImageFilterTest.cxx
itkPathToChainCodePathFilterTest.cxx
itkFourierSeriesPathTest.cxx
itkPathFunctionsTest.cxx
itkPolyLineParametricPathTest.cxx
itkChainCodeToFourierSeriesPathFilterTest.cxx
itkExtractOrthogonalSwath2DImageFilterTest.cxx
itkOrthogonalSwath2DPathFilterTest.cxx
)

CreateTestDriver(ITK-Path  "${ITK-Path-Test_LIBRARIES}" "${ITK-PathTests}")

itk_add_test(NAME itkPathHeaderTest
      COMMAND ITK-PathTestDriver itkPathHeaderTest)
itk_add_test(NAME itkPathIteratorTest
      COMMAND ITK-PathTestDriver itkPathIteratorTest)
itk_add_test(NAME itkChainCodePathTest
      COMMAND ITK-PathTestDriver itkChainCodePathTest)
itk_add_test(NAME itkChainCodePath2DTest
      COMMAND ITK-PathTestDriver itkChainCodePath2DTest)
itk_add_test(NAME itkOrthogonallyCorrected2DParametricPathTest
      COMMAND ITK-PathTestDriver itkOrthogonallyCorrected2DParametricPathTest)
itk_add_test(NAME itkPathToImageFilterTest
      COMMAND ITK-PathTestDriver itkPathToImageFilterTest)
itk_add_test(NAME itkPathToChainCodePathFilterTest
      COMMAND ITK-PathTestDriver itkPathToChainCodePathFilterTest)
itk_add_test(NAME itkFourierSeriesPathTest
      COMMAND ITK-PathTestDriver itkFourierSeriesPathTest)
itk_add_test(NAME itkPathFunctionsTest
      COMMAND ITK-PathTestDriver itkPathFunctionsTest)
itk_add_test(NAME itkPolyLineParametricPathTest
      COMMAND ITK-PathTestDriver itkPolyLineParametricPathTest)
itk_add_test(NAME itkChainCodeToFourierSeriesPathFilterTest
      COMMAND ITK-PathTestDriver itkChainCodeToFourierSeriesPathFilterTest)
itk_add_test(NAME itkExtractOrthogonalSwath2DImageFilterTest
      COMMAND ITK-PathTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ExtractOrthogonalSwath2DImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
    itkExtractOrthogonalSwath2DImageFilterTest ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png)
itk_add_test(NAME itkOrthogonalSwath2DPathFilterTest
      COMMAND ITK-PathTestDriver itkOrthogonalSwath2DPathFilterTest)
