set(
  ITKIOMINC_SRC
  itkMINCImageIO.cxx
  itkMINCImageIOFactory.cxx
)

itk_module_add_library(
  ITKIOMINC
  ${ITKIOMINC_SRC}
)
