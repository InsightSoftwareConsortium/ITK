set(ITKIOJPEG_SRC
itkJPEGImageIOFactory.cxx
itkJPEGImageIO.cxx
)

add_library(ITKIOJPEG ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOJPEG_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOJPEG)
