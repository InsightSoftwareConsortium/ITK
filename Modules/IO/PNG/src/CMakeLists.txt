set(ITKIOPNG_SRCS
  itkPNGImageIOFactory.cxx
  itkPNGImageIO.cxx
  )

itk_module_add_library(ITKIOPNG ${ITKIOPNG_SRCS})
