set(ITKPolynomials_SRC
itkMultivariateLegendrePolynomial.cxx
)

add_library(ITKPolynomials ${ITKPolynomials_SRC})
itk_module_link_dependencies()
itk_module_target(ITKPolynomials)
