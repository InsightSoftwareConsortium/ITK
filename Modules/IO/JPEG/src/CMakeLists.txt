set(ITKIOJPEG_SRCS
  itkJPEGImageIOFactory.cxx
  itkJPEGImageIO.cxx
  )

itk_module_add_library(ITKIOJPEG ${ITKIOJPEG_SRCS})
