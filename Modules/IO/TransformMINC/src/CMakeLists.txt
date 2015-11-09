set(ITKIOTransformMINC_SRC
  itkMINCTransformIOFactory.cxx
)

add_library(ITKIOTransformMINC ${ITKIOTransformMINC_SRC})

itk_module_link_dependencies()

itk_module_target(ITKIOTransformMINC)
