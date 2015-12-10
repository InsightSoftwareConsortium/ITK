set(ITKIOMRC_SRC
itkMRCHeaderObject.cxx
itkMRCImageIO.cxx
itkMRCImageIOFactory.cxx
)

add_library(ITKIOMRC ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOMRC_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOMRC)
