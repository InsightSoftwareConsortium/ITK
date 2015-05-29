set(ITKIOVTK_SRC
itkVTKImageIOFactory.cxx
itkVTKImageIO.cxx
)

add_library(ITKIOVTK ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOVTK_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOVTK)
