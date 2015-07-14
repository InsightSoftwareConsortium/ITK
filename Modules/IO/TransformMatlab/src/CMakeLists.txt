set(ITKIOTransformMatlab_SRC
itkMatlabTransformIOFactory.cxx
)

add_library(ITKIOTransformMatlab ${ITKIOTransformMatlab_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOTransformMatlab)
