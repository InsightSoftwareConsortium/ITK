set(ITKIOTransformMINC_SRC
  itkMINCTransformIO.cxx
  itkMINCTransformIOFactory.cxx
)

add_library(ITKIOTransformMINC ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOTransformMINC_SRC})

itk_module_link_dependencies()

itk_module_target(ITKIOTransformMINC)
