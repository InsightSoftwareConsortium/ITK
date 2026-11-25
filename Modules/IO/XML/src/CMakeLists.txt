set(
  ITKIOXML_SRCS
  itkDOMNode.cxx
  itkDOMNodeXMLReader.cxx
  itkDOMNodeXMLWriter.cxx
  itkFancyString.cxx
  itkStringTools.cxx
  itkXMLFile.cxx
)

itk_module_add_library(ITKIOXML ${ITKIOXML_SRCS})
