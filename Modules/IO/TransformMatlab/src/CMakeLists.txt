set(ITKIOTransformMatlab_SRC
itkMatlabTransformIO.cxx
itkMatlabTransformIOFactory.cxx
)

add_library(ITKIOTransformMatlab ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOTransformMatlab_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOTransformMatlab)
