set(ITKIOXML_SRC
itkXMLFile.cxx
)

add_library(ITKIOXML ${ITKIOXML_SRC})
target_link_libraries(ITKIOXML  ${ITKExpat_LIBRARIES} ${ITKIOBase_LIBRARIES})
itk_module_target(ITKIOXML)
