set(
  ITKIOTIFF_SRCS
  itkTIFFImageIO.cxx
  itkTIFFImageIOFactory.cxx
  itkTIFFReaderInternal.cxx
)

itk_module_add_library(ITKIOTIFF ${ITKIOTIFF_SRCS})
