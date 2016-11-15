set(ITKBiasCorrection_SRCS
  itkCompositeValleyFunction.cxx
  itkCacheableScalarFunction.cxx
  )

itk_module_add_library(ITKBiasCorrection ${ITKBiasCorrection_SRCS})
