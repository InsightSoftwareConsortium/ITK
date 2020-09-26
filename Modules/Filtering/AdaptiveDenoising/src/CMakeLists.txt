set(AdaptiveDenoising_SRCS
  itkMinimalStandardRandomVariateGenerator.cxx
  )

itk_module_add_library(AdaptiveDenoising ${AdaptiveDenoising_SRCS})
