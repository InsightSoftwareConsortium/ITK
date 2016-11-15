set(ITKIOGIPL_SRCS
  itkGiplImageIOFactory.cxx
  itkGiplImageIO.cxx
  )

itk_module_add_library(ITKIOGIPL ${ITKIOGIPL_SRCS})
