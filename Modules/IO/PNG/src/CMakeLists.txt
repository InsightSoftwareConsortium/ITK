set(ITKIOPNG_SRC
itkPNGImageIOFactory.cxx
itkPNGImageIO.cxx
)

add_library(ITKIOPNG ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOPNG_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOPNG)
