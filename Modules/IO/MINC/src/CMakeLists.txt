set(ITKIOMINC_SRC
  itkMINCImageIO.cxx
  itkMINCImageIOFactory.cxx
)

add_library(ITKIOMINC ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOMINC_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOMINC)
