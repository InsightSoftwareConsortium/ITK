set(ITKPolynomials_SRC
itkMultivariateLegendrePolynomial.cxx
)

add_library(ITKPolynomials ${ITKPolynomials_SRC})
target_link_libraries(ITKPolynomials  ${ITKCommon_LIBRARIES})
itk_module_target(ITKPolynomials)
