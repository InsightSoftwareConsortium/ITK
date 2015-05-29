set(ITKBiasCorrection_SRC
itkCompositeValleyFunction.cxx
itkCacheableScalarFunction.cxx
)

add_library(ITKBiasCorrection ${ITKBiasCorrection_SRC})
itk_module_link_dependencies()
itk_module_target(ITKBiasCorrection)
