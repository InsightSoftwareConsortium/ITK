set(
  ITKIOPNG_SRCS
  itkPNGImageIO.cxx
  itkPNGImageIOFactory.cxx
)

itk_module_add_library(ITKIOPNG ${ITKIOPNG_SRCS})
