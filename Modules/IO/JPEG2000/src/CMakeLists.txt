set(ITKIOJPEG2000_SRCS
  itkJPEG2000ImageIOFactory.cxx
  itkJPEG2000ImageIO.cxx
  )

itk_module_add_library(ITKIOJPEG2000 ${ITKIOJPEG2000_SRCS})
