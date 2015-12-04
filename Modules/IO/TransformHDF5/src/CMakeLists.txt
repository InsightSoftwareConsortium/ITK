set(ITKIOTransformHDF5_SRC
itkHDF5TransformIO.cxx
itkHDF5TransformIOFactory.cxx
)

add_library(ITKIOTransformHDF5 ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOTransformHDF5_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOTransformHDF5)
