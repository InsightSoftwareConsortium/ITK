set(ITKIOXML_SRCS
  itkXMLFile.cxx
  itkDOMNode.cxx
  itkDOMNodeXMLReader.cxx
  itkDOMNodeXMLWriter.cxx
  itkStringTools.cxx
  itkFancyString.cxx
  )

itk_module_add_library(ITKIOXML ${ITKIOXML_SRCS})
