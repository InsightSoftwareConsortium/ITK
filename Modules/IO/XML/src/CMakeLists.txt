set(ITKIOXML_SRC
  itkXMLFile.cxx
  itkDOMNode.cxx
  itkDOMNodeXMLReader.cxx
  itkDOMNodeXMLWriter.cxx
  itkStringTools.cxx
  itkFancyString.cxx
)

add_library(ITKIOXML ${ITKIOXML_SRC})
target_link_libraries(ITKIOXML  ${ITKExpat_LIBRARIES} ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOXML)
