set(ITKBiasCorrection_SRC
itkCompositeValleyFunction.cxx
itkCacheableScalarFunction.cxx
)

add_library(ITKBiasCorrection ${ITKBiasCorrection_SRC})
target_link_libraries(ITKBiasCorrection  ${ITKImageFilterBase_LIBRARIES} ${ITKPolynomials_LIBRARIES} ${ITKStatistics_LIBRARIES} ${ITKOptimizers_LIBRARIES})
itk_module_target(ITKBiasCorrection)
