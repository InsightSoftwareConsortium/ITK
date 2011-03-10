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
)

CreateTestDriver(ITK-Path  "${ITK-Path-Test_LIBRARIES}" "${ITK-PathTests}")

add_test(NAME itkPathHeaderTest
      COMMAND ITK-PathTestDriver itkPathHeaderTest)
add_test(NAME itkPathIteratorTest
      COMMAND ITK-PathTestDriver itkPathIteratorTest)
add_test(NAME itkChainCodePathTest
      COMMAND ITK-PathTestDriver itkChainCodePathTest)
add_test(NAME itkChainCodePath2DTest
      COMMAND ITK-PathTestDriver itkChainCodePath2DTest)
add_test(NAME itkOrthogonallyCorrected2DParametricPathTest
      COMMAND ITK-PathTestDriver itkOrthogonallyCorrected2DParametricPathTest)
add_test(NAME itkPathToImageFilterTest
      COMMAND ITK-PathTestDriver itkPathToImageFilterTest)
add_test(NAME itkPathToChainCodePathFilterTest
      COMMAND ITK-PathTestDriver itkPathToChainCodePathFilterTest)
add_test(NAME itkFourierSeriesPathTest
      COMMAND ITK-PathTestDriver itkFourierSeriesPathTest)
add_test(NAME itkPathFunctionsTest
      COMMAND ITK-PathTestDriver itkPathFunctionsTest)
add_test(NAME itkPolyLineParametricPathTest
      COMMAND ITK-PathTestDriver itkPolyLineParametricPathTest)
add_test(NAME itkChainCodeToFourierSeriesPathFilterTest
      COMMAND ITK-PathTestDriver itkChainCodeToFourierSeriesPathFilterTest)
