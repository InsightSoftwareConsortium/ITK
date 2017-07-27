set(BoneMorphometry_SRCS
  itkMinimalStandardRandomVariateGenerator.cxx
  )

add_library(BoneMorphometry ${BoneMorphometry_SRCS})
itk_module_link_dependencies()
itk_module_target(BoneMorphometry)
