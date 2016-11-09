set(ITKIOTIFF_SRCS
  itkTIFFImageIO.cxx
  itkTIFFReaderInternal.cxx
  itkTIFFImageIOFactory.cxx
  )

itk_module_add_library(ITKIOTIFF ${ITKIOTIFF_SRCS})
