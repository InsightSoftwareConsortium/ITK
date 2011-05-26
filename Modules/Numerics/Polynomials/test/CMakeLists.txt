itk_module_test()
set(ITK-PolynomialsTests
itkMultivariateLegendrePolynomialTest.cxx
itkPolynomialsHeaderTest.cxx
)

CreateTestDriver(ITK-Polynomials  "${ITK-Polynomials-Test_LIBRARIES}" "${ITK-PolynomialsTests}")

add_test(NAME itkPolynomialsHeaderTest
      COMMAND ITK-PolynomialsTestDriver itkPolynomialsHeaderTest)
add_test(NAME itkMultivariateLegendrePolynomialTest
      COMMAND ITK-PolynomialsTestDriver itkMultivariateLegendrePolynomialTest)
