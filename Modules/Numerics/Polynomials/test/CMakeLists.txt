itk_module_test()
set(ITKPolynomialsGTests itkMultivariateLegendrePolynomialGTest.cxx)
creategoogletestdriver(ITKPolynomials "${ITKPolynomials-Test_LIBRARIES}" "${ITKPolynomialsGTests}")
