set(
  ITKBiasCorrection_SRCS
  itkCacheableScalarFunction.cxx
  itkCompositeValleyFunction.cxx
)

itk_module_add_library(ITKBiasCorrection ${ITKBiasCorrection_SRCS})
