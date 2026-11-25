set(
  ITKIOJPEG_SRCS
  itkJPEGImageIO.cxx
  itkJPEGImageIOFactory.cxx
)

itk_module_add_library(ITKIOJPEG ${ITKIOJPEG_SRCS})
