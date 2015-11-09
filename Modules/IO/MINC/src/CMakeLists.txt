set(ITKIOMINC_SRC
  itkMINCImageIO.cxx
  itkMINCImageIOFactory.cxx
)

add_library(ITKIOMINC ${ITKIOMINC_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOMINC)
