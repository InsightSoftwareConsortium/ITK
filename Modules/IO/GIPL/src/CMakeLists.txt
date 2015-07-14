set(ITKIOGIPL_SRC
itkGiplImageIOFactory.cxx
itkGiplImageIO.cxx
)

add_library(ITKIOGIPL ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOGIPL_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOGIPL)
