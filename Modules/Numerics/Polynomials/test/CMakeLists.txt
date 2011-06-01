itk_module_test()
set(ITK-PolynomialsTests
itkMultivariateLegendrePolynomialTest.cxx
itkPolynomialsHeaderTest.cxx
)

CreateTestDriver(ITK-Polynomials  "${ITK-Polynomials-Test_LIBRARIES}" "${ITK-PolynomialsTests}")

itk_add_test(NAME itkPolynomialsHeaderTest
      COMMAND ITK-PolynomialsTestDriver itkPolynomialsHeaderTest)
itk_add_test(NAME itkMultivariateLegendrePolynomialTest
      COMMAND ITK-PolynomialsTestDriver itkMultivariateLegendrePolynomialTest)
