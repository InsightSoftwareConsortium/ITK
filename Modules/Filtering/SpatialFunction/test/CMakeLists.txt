itk_module_test()
set(ITKSpatialFunctionTests
itkSpatialFunctionImageEvaluatorFilterTest.cxx
itkSpatialFunctionHeaderTest.cxx
)

CreateTestDriver(ITKSpatialFunction  "${ITKSpatialFunction-Test_LIBRARIES}" "${ITKSpatialFunctionTests}")

itk_add_test(NAME itkSpatialFunctionHeaderTest
      COMMAND ITKSpatialFunctionTestDriver itkSpatialFunctionHeaderTest)
itk_add_test(NAME itkSpatialFunctionImageEvaluatorFilterTest
      COMMAND ITKSpatialFunctionTestDriver itkSpatialFunctionImageEvaluatorFilterTest)
