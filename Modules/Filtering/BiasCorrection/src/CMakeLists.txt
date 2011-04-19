set(ITK-BiasCorrection_SRC
itkCompositeValleyFunction.cxx
itkCacheableScalarFunction.cxx
)

add_library(ITK-BiasCorrection ${ITK-BiasCorrection_SRC})
target_link_libraries(ITK-BiasCorrection  ${ITK-ImageFilterBase_LIBRARIES} ${ITK-Polynomials_LIBRARIES} ${ITK-Statistics_LIBRARIES} ${ITK-Optimizers_LIBRARIES})
itk_module_target(ITK-BiasCorrection)
