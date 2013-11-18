set(ITKIOMRC_SRC
itkMRCHeaderObject.cxx
itkMRCImageIO.cxx
itkMRCImageIOFactory.cxx
)

add_library(ITKIOMRC ${ITKIOMRC_SRC})
target_link_libraries(ITKIOMRC  ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOMRC)
