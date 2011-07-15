itk_module_test()
set(ITKPolynomialsTests
itkMultivariateLegendrePolynomialTest.cxx
itkPolynomialsHeaderTest.cxx
)

CreateTestDriver(ITKPolynomials  "${ITKPolynomials-Test_LIBRARIES}" "${ITKPolynomialsTests}")

itk_add_test(NAME itkPolynomialsHeaderTest
      COMMAND ITKPolynomialsTestDriver itkPolynomialsHeaderTest)
itk_add_test(NAME itkMultivariateLegendrePolynomialTest
      COMMAND ITKPolynomialsTestDriver itkMultivariateLegendrePolynomialTest)
