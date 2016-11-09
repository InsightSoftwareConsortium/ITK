set(ITKIOBMP_SRCS
  itkBMPImageIO.cxx
  itkBMPImageIOFactory.cxx
  )

itk_module_add_library(ITKIOBMP ${ITKIOBMP_SRCS})
