itk_module_test()
set(ITK-SpatialFunctionTests
itkSpatialFunctionImageEvaluatorFilterTest.cxx
itkSpatialFunctionHeaderTest.cxx
)

CreateTestDriver(ITK-SpatialFunction  "${ITK-SpatialFunction-Test_LIBRARIES}" "${ITK-SpatialFunctionTests}")

itk_add_test(NAME itkSpatialFunctionHeaderTest
      COMMAND ITK-SpatialFunctionTestDriver itkSpatialFunctionHeaderTest)
itk_add_test(NAME itkSpatialFunctionImageEvaluatorFilterTest
      COMMAND ITK-SpatialFunctionTestDriver itkSpatialFunctionImageEvaluatorFilterTest)
