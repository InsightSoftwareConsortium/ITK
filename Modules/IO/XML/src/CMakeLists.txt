set(ITK-IO-XML_SRC
itkXMLFile.cxx
)

add_library(ITK-IO-XML ${ITK-IO-XML_SRC})
target_link_libraries(ITK-IO-XML  ${ITK-Expat_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-XML)
