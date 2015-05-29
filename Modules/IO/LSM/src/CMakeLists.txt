set(ITKIOLSM_SRC
itkLSMImageIOFactory.cxx
itkLSMImageIO.cxx
)

add_library(ITKIOLSM ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOLSM_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOLSM)
