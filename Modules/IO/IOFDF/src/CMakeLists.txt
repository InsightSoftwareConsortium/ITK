set(
  IOFDF_SRC
  itkFDFImageIO.cxx
  itkFDFCommonImageIO.cxx
  itkFDFImageIOFactory.cxx
)

itk_module_add_library(IOFDF ${IOFDF_SRC})
