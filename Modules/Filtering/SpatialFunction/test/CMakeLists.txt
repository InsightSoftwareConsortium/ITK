itk_module_test()
set(ITKSpatialFunctionTests
itkSpatialFunctionImageEvaluatorFilterTest.cxx
)

CreateTestDriver(ITKSpatialFunction  "${ITKSpatialFunction-Test_LIBRARIES}" "${ITKSpatialFunctionTests}")

itk_add_test(NAME itkSpatialFunctionImageEvaluatorFilterTest
      COMMAND ITKSpatialFunctionTestDriver itkSpatialFunctionImageEvaluatorFilterTest)
