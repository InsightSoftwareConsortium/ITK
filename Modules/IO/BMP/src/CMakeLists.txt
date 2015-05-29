set(ITKIOBMP_SRC
itkBMPImageIO.cxx
itkBMPImageIOFactory.cxx
)

add_library(ITKIOBMP ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOBMP_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOBMP)
