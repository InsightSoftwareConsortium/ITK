itk_module_test()
set(ITKPolynomialsTests itkMultivariateLegendrePolynomialTest.cxx)

createtestdriver(ITKPolynomials "${ITKPolynomials-Test_LIBRARIES}" "${ITKPolynomialsTests}")

itk_add_test(
  NAME
  itkMultivariateLegendrePolynomialTest
  COMMAND
  ITKPolynomialsTestDriver
  itkMultivariateLegendrePolynomialTest)
