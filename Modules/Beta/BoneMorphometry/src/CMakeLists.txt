set(ModuleTemplate_SRCS
  itkMinimalStandardRandomVariateGenerator.cxx
  )

add_library(ModuleTemplate ${ModuleTemplate_SRCS})
itk_module_link_dependencies()
itk_module_target(ModuleTemplate)
